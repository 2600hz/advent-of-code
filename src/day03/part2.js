import { pipe, sum } from '../utils.js';
import { readWirePaths, nextPoint, wirePathsToPointsGenerator, intersects } from './helpers.js';

/**
 * --- Part Two ---
 * It turns out that this circuit is very timing-sensitive; you actually need to minimize the signal
 * delay.
 *
 * To do this, calculate the number of steps each wire takes to reach each intersection; choose the
 * intersection where the sum of both wires' steps is lowest. If a wire visits a position on the
 * grid multiple times, use the steps value from the first time it visits that position when
 * calculating the total value of a specific intersection.
 *
 * The number of steps a wire takes is the total number of grid squares the wire has entered to get
 * to that location, including the intersection being considered. Again consider the example from
 * above:
 *
 * ...........
 * .+-----+...
 * .|.....|...
 * .|..+--X-+.
 * .|..|..|.|.
 * .|.-X--+.|.
 * .|..|....|.
 * .|.......|.
 * .o-------+.
 * ...........
 *
 * In the above example, the intersection closest to the central port is reached after
 * 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the second wire for a total of
 * 20+20 = 40 steps.
 *
 * However, the top-right intersection is better: the first wire takes only 8+5+2 = 15 and the
 * second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.
 *
 * Here are the best steps for the extra examples from above:
 *
 * - R75,D30,R83,U83,L12,D49,R71,U7,L72
 *   U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
 * - R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
 *   U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
 *
 * What is the fewest combined steps the wires must take to reach an intersection?
 */

const addPoints = function addPoints(...[direction, distance, current, points = []]) {
  if (!distance) return [current, points];
  const [,, steps] = current;
  const [x, y] = nextPoint(direction, current);
  const next = [x, y, steps + 1];
  points.push(next)
  return addPoints(direction, distance - 1, next, points);
};

const wirePathToPoints = function wirePathToPoints(...[
  wirePath,
  current = [0, 0, 0],
  points = []
]) {
  if (!wirePath.length) return points;
  const [[direction, distance]] = wirePath;
  const [next, newPoints] = addPoints(direction, distance, current, points);
  return wirePathToPoints(wirePath.splice(1), next, newPoints);
};

const wirePathsToPoints = wirePathsToPointsGenerator(wirePathToPoints);

const resolveIntersections = ([wire1Points, wire2Points]) =>
  wire1Points.reduce((acc, [x1, y1, steps1]) => {
    const point2Index = wire2Points.findIndex(intersects([x1, y1]));
    const [,, steps2] = wire2Points[point2Index] || [];
    return point2Index < 0 ? acc : [...acc, [x1, y1, sum(steps1, steps2)]];
  }, []);

const findLeastSteps = intersections =>
  intersections.reduce((acc, [,, steps]) => acc > steps ? steps : acc, Infinity);

// :: String -> Promise(Number)
const intersection = pipe(
  readWirePaths,
  wirePathsToPoints,
  resolveIntersections,
  findLeastSteps,
  console.log
);

intersection(import.meta.url).catch(console.log);
