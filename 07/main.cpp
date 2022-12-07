#include<iostream>
#include<string>
#include<list>
#include<regex>
#include<memory>
#include<climits>

using namespace std;

// Push directories onto a stack as they appear, 
// pop the stack and move the popped item to a "processed" list when encountering a "cd ..".
// Any time a size is encountered, increment the size of all directories on the stack.
// At end of input, pop all remaining items on stack onto the "processed" list.
// The "processed" list should contain each directory and its size.

class Directory {
   public:
      Directory(string);
      long size;
      string name;
};

Directory::Directory(string name){
   this->name = name;
   this->size = 0;
}

int main(int argc, char** argv){
   string line;
   smatch m;
   list<shared_ptr<Directory>> processing;
   list<shared_ptr<Directory>> processed;
   list<shared_ptr<Directory>>::iterator it;


   while(getline(cin, line)){
      if(regex_match(line, regex("^\\$ cd \\.\\..*"))){
         processed.push_back(processing.back());
         processing.pop_back();
      }
      else if(regex_match(line, m, regex("^\\$ cd (.*)"))){
         processing.push_back(make_shared<Directory>(m[1]));
      }
      else if(regex_match(line, m, regex("^([0-9]+) .*"))){
         // This is why we had to use a list instead of just a stack for processing.
         // You can't iterate over a stack without "destroying" it by popping.
         for(it = processing.begin(); it != processing.end(); it++){
            (*it)->size += stol(m[1]);
         }
      }
   }

   while(!processing.empty()){
      processed.push_back(processing.back());
      processing.pop_back();
   }

   long total = 0;
   long maxDirectorySize = 0;

   for(it = processed.begin(); it != processed.end(); it++){
      long size = (*it)->size;

      if(size <= 100000){
         total+=size;
      }

      // Cheap way to capture the size of the root directory since we're already
      // iterating over the list.
      if(size > maxDirectorySize){
         maxDirectorySize = size;
      }
   }

   cout << "Answer 1: " << total << endl; // 1490523 for my input.txt

   long totalSpace = 70000000;
   long minRequiredSpace = 30000000;

   //                           (space needed)   - (currently available space)
   long requiredCandidateSize = minRequiredSpace - (totalSpace - maxDirectorySize);
   long smallestSuitableCandidateSize = LONG_MAX;
   string directoryName;

   for(it = processed.begin(); it != processed.end(); it++){
      long size = (*it)->size;
      string name = (*it)->name;

      if(size >= requiredCandidateSize && size < smallestSuitableCandidateSize){
         directoryName = name;
         smallestSuitableCandidateSize = size;
      }
   }

   cout << "Answer 2: " << smallestSuitableCandidateSize << endl; // 12390492 for my input.txt
}
