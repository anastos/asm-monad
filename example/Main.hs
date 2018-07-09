{-# LANGUAGE RecursiveDo #-}

module Main where

import Assembly

factorial :: Assembly ()
factorial = mdo
    mov r1 #1
    cmp r0 #1
    ble end
    loop <- label
    mul r1 r0
    sub r0 #1
    cmp r0 #1
    bgt loop
    end <- label
    exit

fac :: Int -> Int
fac n =
    reg r1 $
    execute (mov r0 #n >> factorial)
    initialState

main :: IO ()
main = print $ fac 10

