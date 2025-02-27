#ifndef CINDER_H
#define CINDER_H

#include <stdio.h>
#ifdef _WIN32
#include <windows.h>
#define THREAD HANDLE
#define THREAD_FUNC DWORD WINAPI
#define THREAD_CREATE(tid, func, arg) (*(tid) = CreateThread(NULL, 0, func, arg, 0, NULL), *(tid) ? 0 : -1)
#define THREAD_JOIN(tid) (WaitForSingleObject(tid, INFINITE) == WAIT_FAILED ? -1 : 0)
#else
#include <unistd.h>
#include <pthread.h>
#define THREAD pthread_t
#define THREAD_FUNC void *
#define THREAD_CREATE(tid, func, arg) pthread_create(tid, NULL, func, arg)
#define THREAD_JOIN(tid) pthread_join(tid, NULL)
#endif

#define VERSION "0.1.0"
#define MAX_LINE 1024
#define MAX_RULES 100

typedef struct {
    char target[MAX_LINE];
    char dependency[MAX_LINE];
    char command[MAX_LINE];
} Rule;

bool fileExists(const char *filename);
void replacePlaceholders(char *command, const char *target, const char *dependency);
THREAD_FUNC runBuildRule(void *arg);
bool parseRule(const char *line, char *target, char *dependency);
void parseLines(FILE *file);
void printHelp();

#endif // CINDER_H