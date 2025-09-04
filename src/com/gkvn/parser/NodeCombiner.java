package com.gkvn.parser;

@FunctionalInterface
public interface NodeCombiner<L, O, R, N> {
    N apply(L lhs, O operator, R rhs);
}