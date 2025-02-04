#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#ifdef _WIN32
#include <windows.h>
#define THREAD HANDLE
#define THREAD_FUNC DWORD WINAPI
#define THREAD_CREATE(tid, func, arg) *(tid) = CreateThread(NULL, 0, func, arg, 0, NULL)
#define THREAD_JOIN(tid) WaitForSingleObject(tid, INFINITE)
#else
#include <unistd.h>
#include <pthread.h>
#define THREAD pthread_t
#define THREAD_FUNC void *
#define THREAD_CREATE(tid, func, arg) pthread_create(tid, NULL, func, arg)
#define THREAD_JOIN(tid) pthread_join(tid, NULL)
#endif

#define VERSION "0.0.1"
#define MAX_LINE 1024
#define MAX_RULES 100

typedef struct {
    char target[MAX_LINE];
    char dependency[MAX_LINE];
    char command[MAX_LINE];
} Rule;

Rule rules[MAX_RULES];
int ruleCount = 0;
bool silent = false;
bool dryRun = false;
int maxJobs = 1;

bool fileExists(const char *filename) {
#ifdef _WIN32
    return GetFileAttributes(filename) != INVALID_FILE_ATTRIBUTES;
#else
    return access(filename, F_OK) == 0;
#endif
}

void replacePlaceholders(char *command, const char *target, const char *dependency) {
    char buffer[MAX_LINE];
    char *ptr = buffer;
    
    for (const char *c = command; *c; c++) {
        if (*c == '$' && *(c + 1) == '!') {
            strcpy(ptr, target);
            ptr += strlen(target);
            c++;
        } else if (*c == '$' && *(c + 1) == '@') {
            strcpy(ptr, dependency);
            ptr += strlen(dependency);
            c++;
        } else {
            *ptr++ = *c;
        }
    }
    *ptr = '\0';
    strcpy(command, buffer);
}

THREAD_FUNC runBuildRule(void *arg) {
    Rule *rule = (Rule *)arg;
    if (!fileExists(rule->dependency)) {
        printf("Dependency %s does not exist.\n", rule->dependency);
        return 0;
    }
    
    replacePlaceholders(rule->command, rule->target, rule->dependency);
    if (dryRun) {
        printf("%s\n", rule->command);
    } else {
        if (!silent) printf("Running: %s\n", rule->command);
        system(rule->command);
        if (!silent) printf("Built %s\n", rule->target);
    }
    return 0;
}

bool parseRule(const char *line, char *target, char *dependency) {
    const char *colon = strchr(line, ':');
    if (!colon) return false;
    
    strncpy(target, line, colon - line);
    target[colon - line] = '\0';
    strcpy(dependency, colon + 1);
    
    return true;
}

void parseLines(FILE *file) {
    char line[MAX_LINE];
    char target[MAX_LINE] = "", dependency[MAX_LINE] = "";
    
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0;
        if (strchr(line, ':')) {
            parseRule(line, target, dependency);
        } else if (target[0] && dependency[0]) {
            strcpy(rules[ruleCount].target, target);
            strcpy(rules[ruleCount].dependency, dependency);
            strcpy(rules[ruleCount].command, line);
            ruleCount++;
            target[0] = dependency[0] = '\0';
        }
    }
}

void printHelp() {
    printf("Usage: cinder [options]\n");
    printf("  -j [N], --jobs[=N]       Allow N jobs at once; infinite jobs with no arg.\n");
    printf("  -h, --help               Print this message and exit.\n");
    printf("  -s, --silent, --quiet    Don't echo recipes.\n");
    printf("  -v, --version            Print the version number and exit.\n");
    printf("  -n, --dry-run            Show commands that would be run, without executing them.\n");
}

int main(int argc, char *argv[]) {
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--version") == 0) {
            printf("Cinder Build Version: %s\n", VERSION);
            return 0;
        } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            printHelp();
            return 0;
        } else if (strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--silent") == 0 || strcmp(argv[i], "--quiet") == 0) {
            silent = true;
        } else if (strcmp(argv[i], "-n") == 0 || strcmp(argv[i], "--dry-run") == 0) {
            dryRun = true;
        } else if (strcmp(argv[i], "-j") == 0 || strncmp(argv[i], "--jobs", 6) == 0) {
            if (i + 1 < argc) {
                maxJobs = atoi(argv[++i]);
            } else {
                maxJobs = -1;
            }
        }
    }
    
    const char *files[] = {"Cinderfile", "cinderfile", "cinderFile", "CinderFile"};
    FILE *file = NULL;
    
    for (int i = 0; i < 4; i++) {
        if ((file = fopen(files[i], "r"))) break;
    }
    
    if (!file) {
        printf("No valid build file found.\n");
        return 1;
    }
    
    parseLines(file);
    fclose(file);
    
    THREAD threads[MAX_RULES];
    for (int i = 0; i < ruleCount; i++) {
        THREAD_CREATE(&threads[i], runBuildRule, &rules[i]);
        if (maxJobs > 0 && (i + 1) % maxJobs == 0) {
            for (int j = i - maxJobs + 1; j <= i; j++) {
                THREAD_JOIN(threads[j]);
            }
        }
    }
    
    for (int i = 0; i < ruleCount; i++) {
        THREAD_JOIN(threads[i]);
    }
    
    return 0;
}
