# Advent of Code: Node.js + ES6

This take of AoC makes use of the [Node.js](https://nodejs.org/en/about/) (JavaScript runtime environment) using [ES6](http://www.ecma-international.org/ecma-262/6.0/) features (modules, promises, arrow functions ...) and follow functional programming principles whenever possible.

## Getting Started
These instructions will get you a copy of the project up and running on your local machine, giving you the ability to execute a specific solution or run the full test suite.

### Prerequisites

- [node](https://nodejs.org/en/download/) >= LTS

### Install

Clone the [`advent-of-code`](https://github.com/2600hz/advent-of-code) repository from [@2600Hz](https://github.com/2600hz/), navigate to the project's folder and checkout the branch:

```shell
git clone https://github.com/2600hz/advent-of-code && cd advent-of-code && git checkout -b joris origin/joris
```

Install dependencies:

```shell
npm install
```

### Execute

To execute a specific solution, run the following command:

```shell
node --experimental-modules --no-warnings <path/to/solution>
```

where `<path/to/solution>` is the path to the solution's file you want to execute and `--no-warnings` an optional switch.

!!! Info
    You might ask yourself **Why the heck is this command so long?**.
    The simple answer is that, while Node.js does support ES6 modules fully, it only provides limited interoperability with the existing module format (namely, [CommonJS](https://nodejs.org/api/modules.html)), hence the `--experimental-modules` switch.

### Test

To run the test suite command line runner, use the following command:

```shell
npm test
```
## Code Structure

Folders corresponding to a numbered day contain a file for each part of the exercise. In that folder, a `helpers.js` module regroups reusable methods for the exercise. At the same level than the numbered days folders, a `utils.js` module regroups methods deemed reusable in multiple exercises.

## Authors

* [**Joris Tirado**](https://github.com/azefiel)

## License

This project is licensed under the Unlicense - see the [LICENSE](LICENSE) file for details.