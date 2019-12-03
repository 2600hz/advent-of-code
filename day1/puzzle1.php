<?php

$modules = explode(PHP_EOL, trim(file_get_contents('input1.txt')));
$result = 0;
foreach ($modules as $module) {
    $result += floor($module/3)-2;
}
echo $result.PHP_EOL;