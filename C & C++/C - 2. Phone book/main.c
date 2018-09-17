#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Person {
  int id;
  char *name;
  char *number;
};

struct Notebook {
  int size;
  struct Person *persons;
};

bool check_number(char *str);
bool check_name(char *str);
char * str_to_lower(char *str);
char * clear_number(char *str);
char * scan_word(FILE *inputStream);
int find_by_id(int id);
void add(char *name, char *number);
void change(int id, char *command, char *newValue);
void delete(int id);
bool find_by_number(char *str);
bool find_by_name(char *str);
void update_file();

FILE *source;
struct Notebook notebook;
int current_id;
const char *file_name;

int main(int argc, const char *argv[]) {
  file_name = argv[1];
  source = fopen(file_name, "a+");
  if (source == NULL) {
    perror("Error");
  }

  notebook.size = 0;
  notebook.persons = malloc(notebook.size * sizeof(struct Person));

  int id;
  rewind(source);

  while (fscanf(source, "%d", &id) == 1) {
    notebook.size++;
    notebook.persons = realloc(notebook.persons, notebook.size * sizeof(struct Person));
    notebook.persons[notebook.size - 1].id = id;
    notebook.persons[notebook.size - 1].name = scan_word(source);
    notebook.persons[notebook.size - 1].number = scan_word(source);
  }
  current_id = (notebook.size ? (notebook.persons[notebook.size - 1].id + 1) : 1);

  char *command = malloc(16 * sizeof(char)), *str, *name, *number;
  while (1) {
    scanf("%s", command);

    if (strcmp(command, "find") == 0) {
      str = scan_word(stdin);
      if (check_name(str)) {
        if (!find_by_name(str)) {
          printf("Not found\n");
        }
      } else if (check_number(str)) {
        find_by_number(str);
      } else {
        printf("Unknown parameter\n");
      }
    }
    else if (strcmp(command, "create") == 0) {
      name = scan_word(stdin);
      number = scan_word(stdin);
      if (check_name(name) && check_number(number)) {
        add(name, number);
      }
    }
    else if (strcmp(command, "delete") == 0) {
      scanf("%d", &id);
      delete(id);
    }
    else if (strcmp(command, "change") == 0) {
      scanf("%d%s", &id, command);
      str = scan_word(stdin);
      if (strcmp(command, "number") == 0 && check_number(str)) {
        change(id, command, str);
      } else if (strcmp(command, "name") == 0 && check_name(str)) {
        change(id, command, str);
      } else {
        printf("Error: unknown modifer `%s`\n", command);
      }
    }
    else if (strcmp(command, "exit") == 0) {
      fclose(source);

      free(name);
      free(number);
      free(command);
      free(str);
      free(notebook.persons);

      return 0;
    }
    else {
      printf ("Error: unknown command `%s`\n", command);
    }

    fflush(stdout);
  }
}

bool check_number(char *str) {
  if (strcmp(str, "") == 0) {
    return false;
  }

  int curr_pos = 0;
  bool left_bracket = false;
  bool right_bracket = false;
  bool last_hyphen = false;

  while (str[curr_pos] != '\0') {
    if (isdigit(str[curr_pos])) {
      last_hyphen = false;
    }
    else if (str[curr_pos] == '(') {
      if (left_bracket || right_bracket) {
        return false;
      }
      left_bracket = true;
      last_hyphen = false;
    }
    else if (str[curr_pos] == ')') {
      if (!left_bracket || right_bracket) {
        return false;
      }
      right_bracket = true;
      last_hyphen = false;
    }
    else if (str[curr_pos] == '-') {
      if (last_hyphen) {
        return false;
      }
      last_hyphen = true;
    }
    else if (str[curr_pos] == '+') {
      if (curr_pos > 0) {
        return false;
      }
    }
    else {
      return false;
    }

    curr_pos++;
  }
  
  return true;
}

bool check_name(char *str) {
  if (strcmp(str, "") == 0) {
    return false;
  }

  int curr_pos = 0;
  
  while (str[curr_pos] != '\0') {
    if (!isalpha(str[curr_pos])) {
      return false;
    }
    curr_pos++;
  }
  
  return true;
}

int find_by_id(int id) {
  int i;
  for (i = 0; i < notebook.size; i++) {
    if (notebook.persons[i].id == id) {
      return i;
    }
  }
  return -1;
}

char * str_to_lower(char *str) {
  int curr_pos = 0;
  char *answer = malloc(strlen(str) * sizeof(char));

  while(str[curr_pos] != '\0') {
    answer[curr_pos] = tolower(str[curr_pos]);
    curr_pos++;
  }
  answer[curr_pos] = '\0';

  return answer;
}

char * clear_number(char *str) {
  int curr_pos = 0;
  int curr_pos_ans = 0;

  char *answer = malloc(strlen(str) * sizeof(char));

  while(str[curr_pos] != '\0') {
    if (isdigit(str[curr_pos])) {
      answer[curr_pos_ans] = str[curr_pos];
      curr_pos_ans++;
    }
    curr_pos++;
  }
  answer[curr_pos_ans] = '\0';
  
  return answer;
}

void update_file() {
  freopen(file_name, "w", source);
  int i;
  for (i = 0; i < notebook.size; i++) {
    if (notebook.persons[i].id) {
      fprintf(source, "%d %s %s\n",
        notebook.persons[i].id, notebook.persons[i].name, notebook.persons[i].number);
    }
  }
  freopen(file_name, "a+", source);
  return;
}

bool find_by_number(char *str) {
  int i;
  bool found = false;
  str = clear_number(str);

  for (i = 0; i < notebook.size; i++) {
    if ( notebook.persons[i].id && (strcmp(clear_number(notebook.persons[i].number), str) == 0) ) {
      printf("%d %s %s\n",
        notebook.persons[i].id, notebook.persons[i].name, notebook.persons[i].number);
      found = true;
    }
  }

  if (!found) {
    return false;
  }
  return true;
}

bool find_by_name(char *str) {
  int i;
  bool found = false;
  str = str_to_lower(str);

  for (i = 0; i < notebook.size; i++) {
    if ( notebook.persons[i].id && strstr(str_to_lower(notebook.persons[i].name), str) ) {
      printf("%d %s %s\n",
        notebook.persons[i].id, notebook.persons[i].name, notebook.persons[i].number);
      found = true;
    }
  }

  if (!found) {
    return false;
  }
  return true;
}

void add(char *name, char *number) {
  notebook.size++;
  notebook.persons = realloc(notebook.persons, notebook.size * sizeof(struct Person));
  notebook.persons[notebook.size - 1].id = current_id;
  notebook.persons[notebook.size - 1].name = name;
  notebook.persons[notebook.size - 1].number = number;
  fprintf(source, "%d %s %s\n", current_id++, name, number);
  return;
}

void delete(int id) {
  int i = find_by_id(id);
  if (i >= 0) {
    notebook.persons[i].id = 0;
    update_file();
  }
  return;
}

void change(int id, char *command, char *newValue) {
  int i = find_by_id(id);
  if (i >= 0) {
    if (!strcmp(command, "number")) {
      strcpy(notebook.persons[i].number, newValue);
    }
    else if (!strcmp(command, "name")) {
      strcpy(notebook.persons[i].name, newValue);
    }

    update_file();
  }
  return;
}

char * scan_word(FILE *inputStream) {
  getc(inputStream);
  char *word = malloc(0 * sizeof(char)), c = fgetc(inputStream);
  int i = 0, j = 0;
  while ((c != EOF) && (c != ' ') && (c != '\n')) {
    if (!(i % 128)) {
      j++;
      word = realloc(word, (j * 128) * sizeof(char));
    }
    word[i++] = c;
    c = fgetc(inputStream);
  }
  word[i] = '\0';
  ungetc(' ', inputStream);
  return word;
}
