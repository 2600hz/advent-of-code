import { pipePromise, getDataFromInput } from '../utils.js';

const matchPairItems = string => [...string.matchAll(/(\D)(\d+)/)];

const formatPair = string => {
  let [[, direction, distance]] = matchPairItems(string);
  return [direction.toLowerCase(), Number(distance)];
};

export const dataToWirePaths = pipePromise(
  a => a.split(/\n/gm),
  a => a.map(line => line.
    split(',').
    map(formatPair)
  )
);

export const readWirePaths = pipePromise(
  getDataFromInput,
  dataToWirePaths
);

export const nextPoint = (direction, [x, y]) => ({
  r: (x, y) => [x + 1, y],
  l: (x, y) => [x - 1, y],
  u: (x, y) => [x, y + 1],
  d: (x, y) => [x, y - 1]
})[direction](x, y);

export const wirePathsToPointsGenerator = wirePathToPoints => ([wire1, wire2]) =>
  [wirePathToPoints(wire1), wirePathToPoints(wire2)];

export const intersects = ([x1, y1]) => ([x2, y2]) => x1 === x2 && y1 === y2;
