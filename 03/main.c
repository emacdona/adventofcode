#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int value(char c);
char commonChar(char* a, char* b);
void commonChars(char* buffer, char* a, char* b);

int main(int argc, char* argv[]){
  char* line = NULL;
  size_t len = 0;
  size_t lineSize = 0;
  long metric = 0;
  long metric2 = 0;
  long lineNum = 0;

  // for problem one
  static char sack1[256];
  static char sack2[256];

  // for problem two
  static char sackI[256];
  static char sackII[256];
  static char sackIII[256];
  static char commonCharBuff[256] = "Hello World\0";

  while(getline(&line, &len, stdin) >= 0){
    lineNum++;
    size_t lineLen = strlen(line);

    if(lineLen > 0 && line[lineLen-1] == '\n'){
      line[--lineLen] = '\0';
    }

    size_t rucksackItemCount = lineLen/2;

    sack1[rucksackItemCount]='\0';
    sack2[rucksackItemCount]='\0';

    strncpy(sack1, line, rucksackItemCount);
    strncpy(sack2, line+rucksackItemCount, rucksackItemCount);

    if(lineNum % 3 == 1){
      strncpy(sackI, line, lineLen);
      sackI[lineLen] = '\0';
    }
    else if(lineNum % 3 == 2){
      strncpy(sackII, line, lineLen);
      sackII[lineLen] = '\0';
    }
    else {
      strncpy(sackIII, line, lineLen);
      sackIII[lineLen] = '\0';
      commonChars(commonCharBuff, sackI, sackII);
      metric2 += value(commonChar(commonCharBuff, sackIII));
    }

    /*
    printf("(%zu/%zu) %s: %s - %s; common: %c; score: %i\n",
           rucksackItemCount,
           lineLen,
           line,
           sack1,
           sack2,
           commonChar(sack1, sack2),
           value(commonChar(sack1, sack2)));
    */

    metric += value(commonChar(sack1, sack2));

    free(line);
    line = NULL;
  }
  free(line);

  printf("Answer: %li\n", metric); // 7746 for my input
  printf("Answer2: %li\n", metric2); // 2604 for my input
}

int value(char c){
  return ((int) c) > 96 ? ((int) c) - 96 : ((int) c) - 38;
}

char commonChar(char* a, char* b){
  size_t alen = strlen(a);
  size_t blen = strlen(b);

  for(int i=0; i<alen; i++){
    for(int j=0; j<blen; j++){
      if(a[i] == b[j]){
        return a[i];
      }
    }
  }
}

void commonChars(char* buffer, char* a, char* b){
  size_t alen = strlen(a);
  size_t blen = strlen(b);
  int bufferPos = 0;

  for(int i=0; i<alen; i++){
    for(int j=0; j<blen; j++){
      //printf("<%i,%i>: <%c,%c>\n", i, j, a[i], b[j]);
      if(a[i] == b[j]){
        //printf("\tFOUND:%c\n", a[i]);
        *(buffer + (bufferPos++)) = a[i];
      }
    }
  }
  *(buffer+bufferPos) = '\0';
  //printf("Common Chars: %s\n", buffer);
}
