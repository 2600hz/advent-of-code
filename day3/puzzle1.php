<?php

// Loading the input into an array of strings, using end of lines as a delimiter
$wires = explode(PHP_EOL, trim(file_get_contents('input.txt')));
$wire1 = $wires[0];
$wire2 = $wires[1];

function listWireLocations($wire) {
    $currentLocation = [0, 0];
    $wireLocations = [];
    $wirePaths = explode(',', $wire);
    foreach ($wirePaths as $path) {
        $direction = substr($path, 0, 1);
        $steps = substr($path, 1);

        // WORK IN PROGRESS

    }

    return $wireLocations;
}