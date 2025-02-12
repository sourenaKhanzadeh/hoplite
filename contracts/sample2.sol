// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Sample2 {
    string public name;

    constructor(string memory _name) {
        name = _name;
    }

    function updateName(string memory _name) public {
        name = _name;
    }
}
