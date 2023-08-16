module Main where
main :: IO ()

q1 sum current_num = 
    if current_num < 1000
        then 
        if mod current_num 3 == 0 || mod current_num 5 == 0
            then q1 (sum + current_num) (current_num + 1)
        else q1 sum (current_num + 1)
    else sum

q2 sum prev_num current_num = 
    if current_num < 4000000
        then 
        if mod current_num 2 == 0
            then q2 (sum + current_num) current_num (current_num + prev_num)
        else q2 sum current_num (current_num + prev_num)
    else sum



q3 checked_number current_divisor  = 
    if current_divisor >= checked_number
        then checked_number
    else 
        if mod checked_number current_divisor == 0
            then q3 (div checked_number current_divisor) (current_divisor)
        else q3 checked_number (current_divisor + 1)

check_palindrome_number number = 
    let numberAsString = show number
    in numberAsString == reverse numberAsString


q4 current_num_1 current_num_2 current_product =
    if current_num_2 >= 1000
        then q4 (current_num_1 + 1) 1 current_product
    else
        if current_num_1 >= 1000
            then current_product
        else 
            let result_product = current_num_1 * current_num_2 in
            if check_palindrome_number result_product && result_product > current_product
                then q4 current_num_1 (current_num_2 + 1) (current_num_1 * current_num_2)
            else q4 current_num_1 (current_num_2 + 1) (current_product)

q5 input_number divisor = 
    if divisor > 20
        then input_number
    else 
        if mod input_number divisor == 0
            then q5 input_number (divisor + 1)
        else 
            q5 (input_number + 20) 2

main = do 
    let result = q1 0 3
    print result
    let result2 = q2 0 1 2
    print result2
    let result3 = q3 600851475143 2 
    print result3
    let result4 = q4 1 1 0
    print result4
    let result5 = q5 20 2
    print result5
