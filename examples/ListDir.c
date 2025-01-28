#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>

void list_directory(const char *path) {
    DIR *dir = opendir(path);
    if (dir == NULL) {
        perror("opendir");
        return;
    }

    struct dirent *entry;
    char full_path[1024];

    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        //printf("%s\n", full_path);

        if (entry->d_type == DT_DIR) {
          snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);

            list_directory(full_path);
        }
    }

    closedir(dir);
}

int main() {
    const char *path = ".";
    list_directory(path);
    return 0;
}
