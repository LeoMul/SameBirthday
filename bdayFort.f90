program main 
    !gfortran bdayFort.f90 -O3 -o bdayFort.x 
    !./bdayFort.x
    !beware of any bugs that my compiler didnt catch but youurs might
    !LPM 12.05.25

    !input file is inp
    !first number is num sampls
    !second number is maxsize,
    !the party sizes are 2,3,4,..... ,maxsize.
    !better coding could make this input more flexible. 

    implicit none
    !parameters 
    integer, parameter :: numDaysYear = 365 
    integer,allocatable :: numpeoplearray(:)
    integer :: maxsize,numsamples
    real*8,allocatable :: avarray(:),vararray(:)

    integer :: ii 

    !todo, put an exception here.
    open(1,file='inp')
        read(1,*) numsamples
        read(1,*) maxsize
    close(1)

    print*, '#',numsamples, maxsize

    allocate(numpeoplearray(maxsize))
    allocate(avarray(maxsize))
    allocate(vararray(maxsize))

    !trivial but expandable 
    do ii=1,maxsize
        numpeoplearray(ii) = ii+1
    end do 

    do ii = 1,size(numpeoplearray)
        call getSamples(numpeoplearray(ii),&
                        avarray(ii),&
                        vararray(ii))
    end do 



    contains 

    subroutine getSamples(num,av,va)
        implicit none
        real*8 :: av,va 
        integer :: ii, jj ,num,check,sum
        integer,allocatable :: birthdays(:) 

        allocate(birthdays(num))
        sum=0
        !main work loop.
        !todo: openmp the loop here 
        do ii = 1,numsamples
            
            !do each ssample.
            do jj = 1,num
                call randint(birthdays(jj))
            end do 
            check = checkForDups(birthdays,num)
            !print*,check
            sum = sum + check
            

        end do 
        !print*,sum
        av = real(sum,8)/real(numsamples,8)
        va = av - av*av
        print*,num,av,va
    end subroutine getSamples

    function checkForDups(array,numpeople)
        implicit none
        integer,intent(in) :: array(:),numpeople
        integer :: checkForDups 
        
        integer :: alreadyFound(numDaysYear)
        integer :: numAlreadyFound 
        integer :: ii, jj

        !preparing a very basic hash map 
        !means an O(n) check each time.
        numAlreadyFound = 0
        checkForDups = 0 
        alreadyFound = 0

        do ii = 1,numpeople
            if(alreadyFound(array(ii)).gt.0) then 
                checkForDups = 1
                return 
                
            else 
                alreadyFound(array(ii)) = 1
            end if 
        end do 

        
    end function checkForDups


    subroutine randint(random_integer)
        !draws a random integer from 1 to numDaysYear
        integer :: random_integer
        real*8 :: u

        call random_number(u)
        random_integer = floor(numDaysYear * u) + 1

    end subroutine


end program 