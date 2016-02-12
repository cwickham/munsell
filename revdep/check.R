library("devtools")

res <- revdep_check(check_dir = "revdep")
revdep_check_save_summary(res)
