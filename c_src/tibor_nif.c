#include "erl_nif.h"

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

static ERL_NIF_TERM tibor_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Figure out the length of the string
    unsigned length;
    if (!enif_get_list_length(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }
    length++;

    // Get the key
    char* key = malloc(length);
    if (!enif_get_string(env, argv[0], key, length, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    // int shmid;
    // key_t key;
    // char *shm;

    // key = 2234;

    // if ((shmid = shmget(key, 32, 0777)) < 0)
    // {
    //     perror("shmget");
    //     exit(1);
    // }

    // printf("Memory created, ID = %d.\n", shmid);

    // if ((shm = shmat(shmid, NULL, 0)) == (char*)-1)
    // {
    //     perror("shmat");
    //     exit(1);
    // }

    // Allocate the binary
    ErlNifBinary bin;
    if(!enif_alloc_binary(32, &bin)) {
        printf("Bad binary allocation\n");
        return enif_make_badarg(env);
    }

    char* shm = NULL;
    if(strcmp(key, "daniel_is_dumb") == 0) {
        shm = "aaaa";
    } else if(strcmp(key, "screw_freebsd") == 0) {
        shm = "abab";
    } else {
        shm = "aabb";
    }

    memcpy(bin.data, shm, 32);
    
    return enif_make_binary(env, &bin);
}

static ErlNifFunc nif_funcs[] = {
    {"get", 1, tibor_get}
};

ERL_NIF_INIT(tibor_nif, nif_funcs, NULL, NULL, NULL, NULL)