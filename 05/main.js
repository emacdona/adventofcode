#!/usr/bin/env node

import * as readline from 'node:readline/promises';
import { stdin as input } from 'node:process';

// Answer 1 based on my input: RFFFWBPNS
// Answer 2 based on my input: CQQBBJFCS
(function(context){
   var stacks = [];

   readline
      .createInterface({input})
      .on('line', input => {
         // row of stacked items
         if(input.match(/^\s*\[/)){
            var stack = 0;
            for(var i=1; i<input.length; i+=4){
               if(stacks[stack] === undefined){
                  stacks[stack] = [];
               }
               if(input[i] != " "){
                  stacks[stack].push(input[i]);
               }
               stack++;
            }

            /*
            for(var i=0; i<stacks.length; i++){
               console.log(`stack ${i}: ${stacks[i]}`);
            }
            */
         }
         // move instructions
         else if(input.match(/^move /)){
            const [unused, quantity, source, destination] = 
               input.match(/^move\s+(\d+)\s+from\s+(\d+)\s+to\s+(\d+)/);
            //console.log(`${quantity}:${source}:${destination}`);
            
            // Uncomment to get answer for #1
            //for(var i=0; i<quantity; i++){
            //   stacks[destination-1].unshift(stacks[source-1].shift());
            //}
            
            // Uncomment to get answer for #2
            var tmp = []
            for(var i=0; i<quantity; i++){
               tmp.push(stacks[source-1].shift());
            }
            for(var i=0; i<quantity; i++){
               stacks[destination-1].unshift(tmp.pop());
            }
         }
      })
      .on('close', () => {
         for(var i = 0; i < stacks.length; i++){
            console.log(stacks[i][0]);
         }
      })
})({})
