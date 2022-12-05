#!/usr/bin/env node

import * as readline from 'node:readline/promises';
import { stdin as input } from 'node:process';

// Answer 1 based on my input: RFFFWBPNS
// Answer 2 based on my input: CQQBBJFCS
(function(context){
   var stacks1 = [];
   var stacks2 = [];

   readline
      .createInterface({input})
      .on('line', input => {
         // row of stacked items
         if(input.match(/^\s*\[/)){
            var stack = 0;
            for(var i=1; i<input.length; i+=4){
               if(stacks1[stack] === undefined){
                  stacks1[stack] = [];
                  stacks2[stack] = [];
               }
               if(input[i] != " "){
                  stacks1[stack].push(input[i]);
                  stacks2[stack].push(input[i]);
               }
               stack++;
            }
         }
         // move instructions
         else if(input.match(/^move /)){
            const [unused, quantity, source, destination] = 
               input.match(/^move\s+(\d+)\s+from\s+(\d+)\s+to\s+(\d+)/);
            
            // Uncomment to get answer for #1
            for(var i=0; i<quantity; i++){
               stacks1[destination-1].unshift(stacks1[source-1].shift());
            }
            
            // Uncomment to get answer for #2
            var tmp = []
            for(var i=0; i<quantity; i++){
               tmp.push(stacks2[source-1].shift());
            }
            for(var i=0; i<quantity; i++){
               stacks2[destination-1].unshift(tmp.pop());
            }
         }
      })
      .on('close', () => {
         console.log("Problem 1:", stacks1.map(x=>x[0]).join(''));
         console.log("Problem 2:", stacks2.map(x=>x[0]).join(''));
      })
})({})
