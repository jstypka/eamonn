#include "erl_nif.h"

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    *priv_data = enif_alloc(sizeof(int));
    return 0;
}

static ERL_NIF_TERM tibor_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_atom(env, "ok");
}

char* concat(char *s1, char *s2)
{
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char *result = malloc(len1+len2+1);//+1 for the zero-terminator
    //in real code you would check for errors in malloc here
    memcpy(result, s1, len1);
    memcpy(result+len1, s2, len2+1);//+1 to copy the null-terminator
    return result;
}

static ERL_NIF_TERM tibor_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
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


    char* filepath = concat("/home/stpk/eamonn_server/", key);

    // Check if the file exists
    if(access(filepath, F_OK) == -1) {
        return enif_make_badarg(env);
    }

    // Get the file size
    struct stat st;
    stat(filepath, &st);
    int filesize = st.st_size;

    // Read the file
    unsigned char* buffer = malloc(filesize);
    int fd = open(filepath, O_RDONLY);
    read(fd, buffer, filesize);

//    char* shm = NULL;
//    if(strcmp(key, "daniel/is/dumb") == 0) {
//        shm = "indeed he is!                   ";
//    } else if(strcmp(key, "screw/freebsd") == 0) {
//        shm = "ubuntu for the win!             ";
//    } else {
//        shm = "Invenio is fun!                 ";
//    }

    // Allocate the binary
    ErlNifBinary bin;
    if(!enif_alloc_binary(filesize, &bin)) {
        printf("Bad binary allocation\n");
        return enif_make_badarg(env);
    }

    memcpy(bin.data, buffer, filesize);
    
    return enif_make_binary(env, &bin);
}

static ErlNifFunc nif_funcs[] = {
    {"init", 0, tibor_init},
    {"get", 1, tibor_get}
};

ERL_NIF_INIT(tibor_nif, nif_funcs, &load, NULL, NULL, NULL)