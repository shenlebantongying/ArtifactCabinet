#!/usr/bin/env node
let i = '';
process.stdin.on('data', c => i += c);
process.stdin.on('end', () => {
    if (i%2===0 && i>2){
        console.log("YES");
    } else {
        console.log("NO");
    }
});