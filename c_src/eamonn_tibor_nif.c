#include "erl_nif.h"

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/msg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>

#define KEY_REQ 111
#define KEY_RES 222

struct tib_request {
	long mtype;
	char key[512];
};
#define TIB_REQUEST_SIZE (sizeof(struct tib_request) - sizeof(long))

struct tib_response {
	long mtype;
	key_t key;
	uint64_t length;
	int success;
};
#define TIB_RESPONSE_SIZE (sizeof(struct tib_response) - sizeof(long))

struct tib_file {
    char* data;
    int size;
};

char* concat(char *s1, char *s2) {
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char *result = malloc(len1 + len2 + 1);//+1 for the zero-terminator
    memcpy(result, s1, len1);
    memcpy(result+len1, s2, len2 + 1);//+1 to copy the null-terminator
    return result;
}

char* get_filepath(ErlNifEnv* env, ERL_NIF_TERM erl_key) {

    // Figure out the length of the string
    unsigned length;
    if (!enif_get_list_length(env, erl_key, &length)) {
        return NULL;
    }
    length++;

    // Get the key
    char* key = malloc(length);
    if (!enif_get_string(env, erl_key, key, length, ERL_NIF_LATIN1)) {
        return NULL;
    }

    char* home_dir = concat(getenv("HOME"), "/eamonn_server/");
    char* filepath = concat(home_dir, key);

    free(key);
    free(home_dir);

    return filepath;
}

struct tib_file load_from_disk(char* filepath) {
    struct tib_file file;

    // Check if the file exists
    if(access(filepath, F_OK) == -1) {
        file.size = -1;
        return file;
    }

    // Get the file size
    struct stat st;
    stat(filepath, &st);
    file.size = st.st_size;

    // Read the file
    file.data = malloc(file.size);
    int fd = open(filepath, O_RDONLY);
    read(fd, file.data, file.size);
    close(fd);

    return file;
}

void make_erl_binary(struct tib_file file, ErlNifBinary* bin) {
    // Allocate the binary
    if(!enif_alloc_binary(file.size, bin)) {
        printf("Bad binary allocation\n");
        bin->size = -1;
        return;
    }

    memcpy(bin->data, file.data, file.size);
}

void make_tib_request(struct tib_request* req, struct tib_response* res) {
    int mq_req, mq_res;

    if ((mq_req = msgget(KEY_REQ, 0)) == -1)
    {
        perror("msgget");
        exit(1);
    }

    if ((mq_res = msgget(KEY_RES, 0)) == -1)
    {
        perror("msgget");
        exit(1);
    }

    //    printf("Sending %d bytes!\n", TIB_REQUEST_SIZE);
    if (msgsnd(mq_req, req, TIB_REQUEST_SIZE, 0) == -1) {
        perror("msgsnd");
        exit(1);
    }

    //    printf("Receiving...\n");
    if (msgrcv(mq_res, res, TIB_RESPONSE_SIZE, req->mtype + 1, 0) == -1) {
        perror("msgrcv");
        exit(1);
    }
}

void get_data(char** shm, struct tib_response* res) {
    int shmid;

    if ((shmid = shmget(res->key, res->length, 0777)) < 0) {
         perror("shmget");
         exit(1);
    }

    if ((*shm = shmat(shmid, NULL, 0)) == (char*)-1) {
         perror("shmat");
         exit(1);
    }
}

static ERL_NIF_TERM tibor_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    static sig_atomic_t mtype = 1;

    char* filepath = get_filepath(env, argv[0]);

    if(!filepath) {
        return enif_make_badarg(env); 
    }

    // Prepare request
    struct tib_request req;
    struct tib_response res;
    memset(req.key, '\0', 512);
    strcpy(req.key, filepath);

    req.mtype = mtype;
    mtype++;

    make_tib_request(&req, &res);
    
    // If file not in the cache
    if (res.success == 0) {
        struct tib_file file = load_from_disk(filepath);
        if(file.size == -1) {
            // File doesn't exist
            return enif_make_badarg(env);
        }

        ErlNifBinary bin;
        make_erl_binary(file, &bin);
        if(bin.size == -1) {
            return enif_make_badarg(env);
        }

        free(file.data);
        free(filepath);
        return enif_make_binary(env, &bin);

    }

    // Get the data from shared memory
    char* shared_memory;
    get_data(&shared_memory, &res);

    struct tib_file file;
    file.size = res.length;
    file.data = shared_memory;

    ErlNifBinary bin;
    make_erl_binary(file, &bin);
    if(bin.size == -1) {
        return enif_make_badarg(env);
    }

    free(filepath);
    return enif_make_binary(env, &bin);
}

static ErlNifFunc nif_funcs[] = {
    {"get", 1, tibor_get}
};

ERL_NIF_INIT(eamonn_tibor_nif, nif_funcs, NULL, NULL, NULL, NULL)