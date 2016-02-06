# 1 "if.F90"
program if

# 5

        implicit none

      integer thread_id
      integer num_threads

      print *,"I am the main thread."

!$omp parallel private(thread_id, num_threads)

# 18

      thread_id = 0
      num_threads = 1


      print *,"Hello. I am thread ", thread_id, &
              " out of a team of ", num_threads
!$omp end parallel

      print *,"Here I am, back to the main thread."

end program if
