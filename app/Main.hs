module Main where
main :: IO ()

q1 sum current_num = 
    if current_num < 1000
        then 
            if mod current_num 3 == 0 || mod current_num 5 == 0
                then q1 (sum + current_num) (current_num + 1)
                else q1 sum (current_num + 1)
        else sum


main = do 
    let result = q1 0 3
    print result
    let result2 = q1 0 3
    print result2
