<?php

$modules = explode(PHP_EOL, trim(file_get_contents('input1.txt')));
$result = 0;
foreach ($modules as $module) {
    $result += calculateFuel($module);
}

echo $result.PHP_EOL;

function calculateFuel($mass) {
    $fuel = floor($mass/3)-2;
    if($fuel > 0) { $fuel += calculateFuel($fuel); }
    else { $fuel = 0; } // If fuel reaches negative value, set back to 0;
    return $fuel;
}