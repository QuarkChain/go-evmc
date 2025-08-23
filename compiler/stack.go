// Copyright 2014 The go-ethereum Authors
// This file is part of the go-ethereum library.
//
// The go-ethereum library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The go-ethereum library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the go-ethereum library. If not, see <http://www.gnu.org/licenses/>.

package compiler

import (
	"github.com/holiman/uint256"
)

// Stack is an object for basic stack operations. Items popped to the stack are
// expected to be changed and modified. stack does not take care of adding newly
// initialized objects.
type Stack struct {
	data     []*uint256.Int
	stackPtr uintptr
}

// Back returns the n'th item in stack in uint256
func (st *Stack) Back(n int) *uint256.Int {
	return st.data[len(st.data)-n-1]
}
