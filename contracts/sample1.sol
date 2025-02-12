// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Sample1 {
    uint public value;

    constructor(uint _value) {
        value = _value;
    }

    function setValue(uint _value) public {
        value = _value;
    }
}
