calc_avg_gpa = function(x) {
  grade_pts = c(4.00, 4.00, 3.67, 3.33, 3.00, 2.67, 2.33, 2.00, 1.67, 1.33, 1.00, 0.67, 0.00)
  return(sum(x * grade_pts) / sum(x))
}
