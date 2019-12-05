<?php

// Loading the input into an array of strings, using commas as a delimiter
// Note: no need to cast those strings into ints, PHP will automagically do that when performing operations
$intcode = explode(',', trim(file_get_contents('input.txt')));

// Setting values at position 1 & 2 as instructed
$intcode[1] = 12;
$intcode[2] = 2;

for ($i=0; $i < (count($intcode)-4); $i+=4) { 
    if($intcode[$i] == 1) {
        $intcode[$intcode[$i+3]] = $intcode[$intcode[$i+1]] + $intcode[$intcode[$i+2]];
    } elseif($intcode[$i] == 2) {
        $intcode[$intcode[$i+3]] = $intcode[$intcode[$i+1]] * $intcode[$intcode[$i+2]];
    } elseif($intcode[$i] == 99) {
        break;
    } else {
        echo "Unknown Opcode at position $i. Intcode terminated.".PHP_EOL;
        die;
    }
}

echo $intcode[0].PHP_EOL;