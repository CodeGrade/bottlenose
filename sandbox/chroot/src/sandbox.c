
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <alloca.h>
#include <string.h>
#include <assert.h>

const char*  DIR_SETUP_CMD    = "/usr/local/bottlenose/scripts/setup-directory.sh sandbox";
const char*  DIR_TEARDOWN_CMD = "/usr/local/bottlenose/scripts/teardown-directory.sh sandbox";
const char*  START_MAKE_CMD   = "/usr/local/bottlenose/scripts/build-assignment.sh";
const char*  GRADING_PREP_CMD = "/usr/local/bottlenose/scripts/grading-prep.sh sandbox";
const char*  START_TEST_CMD   = "/usr/local/bottlenose/scripts/test-assignment.sh";

const time_t TIME_LIMIT  = 300;
const rlim_t MEM_LIMIT   = 768000000;
const rlim_t PROC_LIMIT  = 1024;
const rlim_t FSIZE_LIMIT = 512000000;

int
watchdog_process(pid_t pid)
{
    time_t start_time = time(0);
    int pid2;

    while (1) {
        sleep(2);

        time_t now = time(0);
        if (now > start_time + TIME_LIMIT) {
            printf("Sandbox process has taken too long.\n");
            printf("SIGKILL!\n");
            kill(pid, SIGKILL);
            sleep(1);
        }

        int status;
        if ((pid2 = waitpid(pid, &status, WNOHANG))) {
            if (pid2 == -1) {
                perror("Watchdog got error in waitpid");
                return 1;
            }
            else {
                printf("Sandbox process (%d) terminated.\n", pid2);

                if (WIFEXITED(status)) {
                    printf("Exited with code %d.\n", WEXITSTATUS(status));

                    if (WEXITSTATUS(status) == 0)
                        return 0;
                    else
                        return 1;
                }

                if (WIFSIGNALED(status)) {
                    printf("Killed by signal %d.\n", WTERMSIG(status));
                    return 1;
                }
            }

            break;
        }
    }

    printf("No idea what's going on.\n");
    return 1;
}

int
run_in_sandbox(int uid, const char* cmd)
{
    int pid, rv;
    char tmp[256];

    /* Make sure sandbox can't be used to
     * run code as a real user. */
    if (uid < 5000)
        uid = 5000 + (uid % 5000);

    if ((pid = fork())) {
        return watchdog_process(pid);
    }
    else {
        chdir("sandbox");
        chroot(".");

        struct rlimit as_lim;
        as_lim.rlim_cur = MEM_LIMIT;
        as_lim.rlim_max = MEM_LIMIT;
        setrlimit(RLIMIT_AS, &as_lim);

        struct rlimit np_lim;
        np_lim.rlim_cur = PROC_LIMIT;
        np_lim.rlim_max = PROC_LIMIT;
        setrlimit(RLIMIT_NPROC, &np_lim);

        struct rlimit fs_lim;
        fs_lim.rlim_cur = FSIZE_LIMIT;
        fs_lim.rlim_max = FSIZE_LIMIT;
        setrlimit(RLIMIT_FSIZE, &fs_lim);

        snprintf(tmp, 256, "chown -R %d:%d /home/student", uid, uid);
        rv = system(tmp);
        assert(WEXITSTATUS(rv) == 0);

        setreuid(uid, uid);
        int ne = nice(5);
        assert(ne != -1 && "Renice should succeed");

        chdir("/home/student");
        printf("sandbox: running command\n");

        snprintf(tmp, 256, "(%s) 2>&1", cmd);
        rv = system(tmp);

        printf("sandbox: command exited with status %d\n", WEXITSTATUS(rv));
        if (WEXITSTATUS(rv) != 0)
            exit(1);
        else
            exit(0);
    }
}

void
reap_user(int uid)
{
    char* tmp = alloca(100);
    snprintf(tmp, 100, "ps --no-headers -o pid -u %d | xargs -r -n 1 kill -9", uid);
    system(tmp);
}

void
show_usage()
{
    printf("Usage:\n");
    printf("  sandbox dir uid key\n");
    printf("  sandbox -reap uid\n");
    printf("  sandbox -teardown dir\n");
}

int
main(int argc, char* argv[])
{
    if (argc < 3) {
        show_usage();
        return 1;
    }

    if (argc < 4) {
        if (strcmp(argv[1], "-reap") == 0) {
            int REAP_UID = atoi(argv[2]);
            setreuid(0, 0);
            reap_user(REAP_UID);
            return 0;
        }

        if (strcmp(argv[1], "-teardown") == 0) {
            setreuid(0, 0);
            chdir(argv[2]);
            system(DIR_TEARDOWN_CMD);
            return 0;
        }

        show_usage();
        return 1;
    }

    const char* DIR = argv[1];
    int         UID = atoi(argv[2]);
    const char* KEY = argv[3];

    if (DIR[0] == '-') {
        show_usage();
        return 1;
    }

    setlinebuf(stdout);
    setreuid(0, 0);

    int fail = 0;
    int rv = 0;

    printf("\n== Setting up directory ==\n\n");
    chdir(DIR);
    rv = system(DIR_SETUP_CMD);
    if (WEXITSTATUS(rv) != 0) {
        fail = 1;
        printf("Bad return from DIR_SETUP_CMD\n");
        goto done;
    }

    printf("\n== Building assignment. ==\n\n");
    fail = run_in_sandbox(UID, START_MAKE_CMD);
    if (fail) {
        printf("Bad return from START_MAKE_CMD\n");
        goto done;
    }

    printf("\n== Testing assignment. ==\n\n");
    rv = system(GRADING_PREP_CMD);
    if (WEXITSTATUS(rv) != 0) {
        fail = 1;
        printf("Bad return from GRADING_PREP_CMD\n");
        goto done;
    }

    printf("%s\n", KEY);
    fail = run_in_sandbox(UID, START_TEST_CMD);
    if (fail) {
        printf("Bad return from START_TEST_CMD\n");
        goto done;
    }

    printf("%s\n", KEY);

done:
    reap_user(UID);
    system(DIR_TEARDOWN_CMD);

    return fail;
}

