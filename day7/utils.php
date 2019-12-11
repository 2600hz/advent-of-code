<?php

// Code taken from https://stackoverflow.com/questions/10222835/get-all-permutations-of-a-php-array
// Original source://docstore.mik.ua/orelly/webprog/pcook/ch04
function listPermutations(array $array) {
    $result = [];

    $recurse = function($array, $start_i = 0) use (&$result, &$recurse) {
        if ($start_i === count($array)-1) {
            array_push($result, $array);
        }

        for ($i = $start_i; $i < count($array); $i++) {
            //Swap array value at $i and $start_i
            $t = $array[$i]; $array[$i] = $array[$start_i]; $array[$start_i] = $t;

            //Recurse
            $recurse($array, $start_i + 1);

            //Restore old order
            $t = $array[$i]; $array[$i] = $array[$start_i]; $array[$start_i] = $t;
        }
    };

    $recurse($array);

    return $result;
}
