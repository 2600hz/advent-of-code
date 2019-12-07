<?php

$input = '152085-670283';
$input_range = explode('-', $input);

$result = findPasswords($input_range[0], $input_range[1]);

echo count($result).PHP_EOL;

function findPasswords($min, $max) {
    $matching_passwords = [];
    for ($i=$min; $i <= $max; $i++) { 
        $pwd = (string)$i;
        $double_digit = false;
        $ascending = true;
        $excluded_numbers=[];
        for ($n=1; $n < strlen($pwd); $n++) { 
            if($pwd[$n] == $pwd[$n-1] && !in_array($pwd[$n], $excluded_numbers)) {
                if(isset($pwd[$n+1]) && $pwd[$n] == $pwd[$n+1]) {
                    $excluded_numbers[] = $pwd[$n];
                } else {
                    $double_digit = true; 
                }
            }
            if($pwd[$n] < $pwd[$n-1]) {
                $ascending = false;
                break;
            }
        }
        if($double_digit && $ascending) {
            $matching_passwords[] = $pwd;
        }
    }
    return $matching_passwords;
}