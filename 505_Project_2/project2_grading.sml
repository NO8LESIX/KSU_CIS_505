 (*
3p --- if the program type checks
 *)


 (*
3p --- Simple output(s)
  1p for each of below
 *)

use "C:\\Mahmood\\project2.sml";
 
 
Interpret "output 13" [];
  (* [13] *)

Interpret "output 5; output 7" [];
  (* [5,7] *)

Interpret "output 7; output ((2*3)+4); output (5-1)" [3];
  (* [7,10,4] *)

 (*
4p --- Integer variables
  1p for each of below
 *)

Interpret "var x; output x" [];
  (* [0] *)  

Interpret "var z; z = 3; output z" [];
  (* [3] *)  

Interpret "var x; var y; y = 4; x = (y + 2); output x" [];
  (* [6] *)  

Interpret "var y; var w; y = 4; w = 7; y = (y * w); output y; output w" [];
  (* [28,7] *)  

 (*
2p --- Input 
  1p for each of below
 *)

Interpret "var w; input w; output w" [5];
  (* [5] *)

Interpret "var z; input z; output z; input z; output z" [8,4,2];
  (* [8,4] *)

 (*
2p --- If & While
  1p for each of below
 *)

Interpret "var y; input y; if y {output 6} {output 5}; input y; if y {output 4} {output 3}" [0,2];
  (* [5,4] *)

Interpret "var n; var fac; input n; output n; fac = 1; while n {fac = (fac * n); n = (n-1)}; output fac" [5];
  (* [5,120] *)

 (*
6p --- Arrays, 1 dimensional
  1p for each of below
 *)

Interpret "var B[8]; output B[2]" [];
  (* [0] *)

Interpret "var B[10]; B[5] = 2; output B[5]" [];
  (* [2] *)

Interpret "var A[10]; input A[3]; output (A[3] + 1)" [3,8];
  (* [4] *)

Interpret "var D[10]; input D[(5-2)]; input D[7]; output (D[3] - D[7])" [7,4];
  (* [3] *)

Interpret "var C[20]; var t; input C[3]; input C[4]; t = C[3]; C[3] = C[4]; C[4] = t; output C[3]; output C[4]" [5,8];
  (* [8,5] *)

Interpret "var m; var i; var j; var t; var B[100]; input m; i = 0; while (m - i) {input B[i]; i = (i + 1)}; i = 1; while (m - i) {j = i; while j {if (B[(j-1)] - B[j]){t = B[j]; B[j] = B[(j-1)]; B[(j-1)] = t; j = (j-1)}{j = 0}}; i = (i + 1)}; i = 0; while (m - i) {output B[i]; i = (i + 1)}" [5,12,9,4,16,13];
  (* [4,9,12,13,16] *)


 (*
2p --- Arrays, multi-dimensional
  1p for each of below
 *)

Interpret "var B[7,5]; input B[4,3]; input B[3,4]; input B[3,3]; output (B[3,3] + B[4,3]); output (B[4,3] + B[3,4])" [2,5,12];
  (* [14, 7] *)

Interpret "var A[2,3]; var B[3,2]; var C[2,2]; var i; var j; var k; i = 0; while (2 - i) {k = 0; while (3 - k) {input A[i,k]; k = (k + 1)}; i = (i + 1)}; k = 0; while (3 - k) {j = 0; while (2 - j) {input B[k,j]; j = (j + 1)}; k = (k + 1)}; i = 0; while (2 - i) {j = 0; while (2 - j) {C[i,j] = 0; k = 0; while (3 - k) {C[i,j] = (C[i,j] + (A[i,k] * B[k,j])); k = (k + 1)}; j = (j + 1)}; i = (i + 1)}; i = 0; while (2 - i) {j = 0; while (2 - j) {output C[i,j]; j = (j + 1)}; i = (i + 1)}" [3,4,5, 2,7,6,  1,9, 8,0, 7,3];
 (* [70,42,100,36] *)

 (*
3p --- Exceptions
  1p for the first of below; 0.5p for each of the rest
 *)

Interpret "var w; input w; output w; input w" [8];
  (* error message saying that input stream exhausted *)

Interpret "var A[8]; A[(4-5)] = 3" [];
  (* error message saying that index out of bound *)

Interpret "var B[9]; output B[10]" [4];
  (* error message saying that index out of bound *)
           
Interpret "var B[3,5]; input B[2,6]" [7,8];
  (* error message saying that index out of bound *)

Interpret "var B[3,5]; input B[4,2]" [3];
  (* error message saying that index out of bound *)
           