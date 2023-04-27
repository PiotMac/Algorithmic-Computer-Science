#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int run_background;

char *builtin_str[] = {
  "cd",
  "exit"
};

int lsh_num_builtins() {
  return sizeof(builtin_str) / sizeof(char *);
}

int lsh_cd(char **args)
{
  if (args[1] == NULL) {
    //cd was written without arguments
    fprintf(stderr, "lsh: expected argument to \"cd\"\n");
  } else {
    //chdir() == -1 if unsuccessful
    if (chdir(args[1]) != 0) {
      perror("lsh");
    }
  }
  return 1;
}

int lsh_exit(char **args)
{
  return 0;
}

int (*builtin_func[]) (char **) = {
  &lsh_cd,
  &lsh_exit
};

int lsh_launch(char **args)
{
  pid_t pid;
  int status;
  int numofargs = sizeof(args) / sizeof(args[0]);
  if((run_background = (*args[numofargs-1] == '&')) != 0) {
  	args[--numofargs] = NULL;
  }
  printf("sizeof args: %d\n", sizeof(args));
  printf("arguments: %d\n", numofargs);
  printf("is bg?: %d\n", run_background);
  
  pid = fork();
  if (pid == 0) {
    // Child process
    if (execvp(args[0], args) == -1) {
      perror("lsh");
    }
    exit(EXIT_FAILURE);
  } else if (pid < 0) {
    // Error forking
    perror("lsh");
  } else {
    // Parent process
    if (run_background == 1) {
    	wait(&pid);
    }
    else {
    do {
      //WUNTRACED - return if a child has stopped
      //waitpid(pid, &status, WUNTRACED) - waiting for the child to stop
      waitpid(pid, &status, WUNTRACED);
      //WIFEXITED(status) == true if the child process terminated correctly
      //WIFSIGNALED(status) == true if the child process was terminated by a signal
    } while (!WIFEXITED(status) && !WIFSIGNALED(status));
    }
  }

  return 1;
}

int lsh_execute(char **args)
{
  if (args[0] == NULL) {
    // An empty command was entered.
    return 1;
  }

  //Checking if the input is a built-in command
  for (int i = 0; i < lsh_num_builtins(); i++) {
    //If yes - executing "cd" or "exit" accordingly
    if (strcmp(args[0], builtin_str[i]) == 0) {
      return (*builtin_func[i])(args);
    }
  }

  return lsh_launch(args);
}

/**
   @brief Read a line of input from stdin.
   @return The line from stdin.
 */
char *lsh_read_line(void)
{
  char *line = NULL;
  size_t bufsize = 0;

  if (getline(&line, &bufsize, stdin) == -1) {
    if (feof(stdin)) {
      exit(EXIT_SUCCESS);
    } else  {
      perror("readline");
      exit(EXIT_FAILURE);
    }
  }

  return line;
}

#define LSH_TOK_BUFSIZE 64
#define LSH_TOK_DELIM " \t\r\n\a"
char **lsh_split_line(char *line)
{
  int bufsize = LSH_TOK_BUFSIZE, position = 0;
  char **tokens = malloc(bufsize * sizeof(char*));
  char *token, **tokens_backup;

  if (!tokens) {
    fprintf(stderr, "lsh: allocation error\n");
    exit(EXIT_FAILURE);
  }

  token = strtok(line, LSH_TOK_DELIM);
  while (token != NULL) {
    tokens[position] = token;
    position++;

    if (position >= bufsize) {
      bufsize += LSH_TOK_BUFSIZE;
      tokens_backup = tokens;
      tokens = realloc(tokens, bufsize * sizeof(char*));
      if (!tokens) {
		free(tokens_backup);
        fprintf(stderr, "lsh: allocation error\n");
        exit(EXIT_FAILURE);
      }
    }

    token = strtok(NULL, LSH_TOK_DELIM);
  }
  tokens[position] = NULL;
  return tokens;
}


void lsh_loop(void)
{
  char *line;
  char **args;
  int status;

  do {
    printf("> ");
    line = lsh_read_line(); //reading input
    args = lsh_split_line(line); //splitting input into words
    status = lsh_execute(args); //executing those words

    free(line);
    free(args);
  } while (status);
}

/**
   @brief Main entry point.
   @param argc Argument count.
   @param argv Argument vector.
   @return status code
 */
int main(int argc, char **argv)
{
  // Load config files, if any.

  // Run command loop.
  lsh_loop();

  // Perform any shutdown/cleanup.

  return EXIT_SUCCESS;
}

