(* Addition *)
val prog1 = "var n; var m; input n; input m; output (n+m)"
val in1 = [7, 2]
val out1 = [9]

(* Factorial *)
val prog2 =
"var n; var fac; input n; output n; fac = 1; \
 \while n {fac = (fac * n); n = (n-1)}; output fac"
val in2 = [4]
val out2 = [4, 24]

(* Reversing two inputs' order *)
val prog3 =
"var A[2]; var t; input A[0]; input A[1]; \
  \t = A[0]; A[0] = A[1]; A[1] = t; \
  \output A[0]; output A[1]"
val in3 = [3, 7]
val out3 = [7, 3]

(* Matrix multiplication *)
val prog4 =
"var A[2,3]; var B[3,2]; var C[2,2]; var i; var j; var k; \
   \i = 0; while (2 - i) \
            \{k = 0; while (3 - k) \
                      \{input A[i,k]; k = (k + 1)}; \
                    \i = (i + 1)}; \
   \k = 0; while (3 - k) \
            \{j = 0; while (2 - j) \
                      \{input B[k,j]; j = (j + 1)}; \
                    \k = (k + 1)}; \
   \i = 0; while (2 - i) \
            \{j = 0; while (2 - j) \
                      \{C[i,j] = 0; k = 0; \
                       \while (3 - k) \
                         \{C[i,j] = (C[i,j] + (A[i,k] * B[k,j])); \
                          \k = (k + 1)}; \
                    \j = (j + 1)}; \
             \i = (i + 1)}; \
   \i = 0; while (2 - i) \
            \{j = 0; while (2 - j) \
                       \{output C[i,j]; j = (j + 1)}; \
             \i = (i + 1)}"
val in4 = [3, 4, 5,
           2, 7, 6,

           1, 9,
           8, 0,
           7, 2]
val out4 = [70,  37,
            100, 30]

(* Insertion sort *)
val prog5 =
"var n; var i; var j; var t; \
   \var B[100]; input n; i = 0; \
   \while (n - i) {input B[i]; i = (i + 1)}; \
   \i = 1; \
   \while (n - i) {j = i; \
     \while j { \
        \if (B[(j-1)] - B[j]) \
          \{t = B[j]; B[j] = B[(j-1)]; B[(j-1)] = t; j = (j-1)} \
          \{j = 0}}; \
        \i = (i + 1)}; \
   \i = 0; while (n - i) {output B[i]; i = (i + 1)}"
val in5 = [8,13,3,5,9,7,14,2,10]
val out5 = [2,3,5,7,9,10,13,14]

(* Range of numbers…There may be a built-in function to do this *)
fun range n =
  let
    fun ranger n acc =
      if n > 0 then ranger (n - 1) (n :: acc) else acc
  in
    ranger n nil
  end

val progs = [prog1, prog2, prog3, prog4, prog5]
val inputs = [in1, in2, in3, in4, in5]
val outputs = [out1, out2, out3, out4, out5]

val testoutputs = map (fn (p, i) => Interpret p i)
                      (ListPair.zip (progs, inputs))
val testresults =
  ListPair.zip ((range 5), (map (op =) (ListPair.zip (testoutputs, outputs))))