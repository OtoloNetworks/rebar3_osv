#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dlfcn.h>
#include <limits.h>

#define ARGS_FILE "/etc/beam.args"
#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define ROOTDIR "/otp"
#define BINDIR ROOTDIR "/erts/bin"

int main(int argc, char **argv){
  setenv("HOME", ROOTDIR, false),
  setenv("ROOTDIR", ROOTDIR, false);
  setenv("BINDIR", BINDIR, false);
  setenv("TERM", "vt100-qemu", false);
  setenv("ERL_INETRC", "/etc/default/erlang/inetrc", false);
  setenv("RICKP_DEBUG", "yes", false);

  if (argc != 3) {
    fprintf(stderr, "exactly two arguments required\n");
    exit(1);
  }

  char path[PATH_MAX] = {0};
  snprintf(path, PATH_MAX, "%s/erlexec", BINDIR);

  void *elf_handle = dlopen(path, RTLD_LAZY);

  if (!elf_handle)
    goto err;

  int (*mainfun)(int, char **) = (int (*)(int, char **)) dlsym(elf_handle, "main");

  char *erl_argv[] = {"erlexec", "-args_file", argv[2], "-boot", argv[1], "-mode", "embedded", NULL};

  int result = (mainfun)?mainfun(7, erl_argv):1;
  dlclose(elf_handle);

  if(!mainfun)
    goto err;

  return result;
err:
  fprintf(stderr, "Error spawning erlexec\n");
  return 1;
}
