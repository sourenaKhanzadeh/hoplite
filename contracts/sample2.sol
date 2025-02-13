// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Sample2 {
    uint public value;

    constructor(uint _value) {
        value = _value;
    }

    function setValue(uint _value) public {
        value = _value;
    }

    function getValue() public view returns (uint) {
        return value;
    }

    function addValue(uint _value) public {
        value += _value;
    }

    function getValues() public view returns (uint, uint) {
        return (value, value + 1);
    }
}
