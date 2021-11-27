open Assert

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let test_prog = [ "hw6programs/max_subarray.oat", "",
                 "[15,5,-1,7,14] 0" ]

let provided_tests : suite = [
  (*Test("sum of max subarray", Gradedtests.pass_all_executed_oat_file test_prog);*)
] 


(*

test_arr5 = [-2,-3,4,-1,-2,1,5,-3, 10] # 14

def find_sum (arr): # O(n^2)
	max_sum = -1000
	for i in range(len(arr)):
		cum_sum = arr[i]
		if cum_sum > max_sum:
				max_sum = cum_sum
		for j in range(i+1, len(arr)):
			cum_sum += arr[j]
			if cum_sum > max_sum:
				max_sum = cum_sum
	return max_sum


*)
