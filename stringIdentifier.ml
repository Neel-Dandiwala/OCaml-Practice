(*
Suppose that a variable word exists and is a string.

Define a variable sentence that uses 5 string concatenations to create a string containing 9 times word, separated by commas (',').

This time, experiment with defining local let ... ins to store the partial results.   
*)

let sentence = 
  let word_ = word ^ "," in 
  let word_2 = word_ ^ word_ in
  let word_4 = word_2 ^ word_2 in
  let word_8 = word_4 ^ word_4 in
  let sentence = word_8 ^ word in
  sentence