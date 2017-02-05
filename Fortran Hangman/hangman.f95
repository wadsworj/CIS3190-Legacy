!   Hangman by James Wadsworth 2017 for CIS*3190 - Legacy
!   
!   Based on    GAME OF HANGMAN BY DAVE AHL, DIGITAL
!               BASED ON A BASIC PROGRAM WRITTEN BY KEN AUPPERLE
!                   HALF HALLOW HILLS H.S. DIX HILLS NY
!               CONVERTED TO FORTRAN 77 BY M.WIRTH, APRIL 2012


    program hangman
        implicit none

        ! variable declarations
        integer, parameter :: max_words = 51    ! maximum number of words to be in a file
        integer i, j, ierr                   
        integer number_of_words
        character*20 words(max_words)

        

        call load_file(max_words, words, number_of_words)
        call menu_loop(number_of_words)

    contains



    !  menu_loop()
    !       Function to loop through the main 'menu'
    subroutine menu_loop(number_of_words)
        character selection
        integer count                       ! number of words that have been answered
        integer number_of_words, i, current_word
        real random
        integer used(max_words)                 ! array to track which words have been used
        count = 0

        ! initialize 'used' to 0
        do i = 1,max_words
            used(i) = 0
        end do

        write( *, '(a)' ) "THE GAME OF HANGMAN"
        write( *, '(a)' ) "---"

        do while (count .LT. number_of_words)
            ! select a random word
            call random_number(random)
            current_word = ceiling(random*number_of_words)

            ! test if word already used or not
            if (used(current_word) .EQ. 0 ) then
                count = count + 1
                used(current_word) = 1
                call game_loop( words(current_word) )
            end if

            write( *,* ) "Do you want another word? (Y/N)"
            read ( *, * ) selection

            if (selection .NE. 'y' .AND. selection .NE. 'Y') then
                write ( *, '(a)') "It's been fun! Bye for now."
                exit
            end if
        end do
    end subroutine menu_loop


    !  game_loop()
    !       Function to loop through the playing of a single game
    subroutine game_loop(current_word)
        integer correct, i, j, end_game, number_guesses, chances
        character guess, alphabet(26), current_word*20, drawing(12,12)

        ! initialize variables
        end_game = 0
        number_guesses = 0
        chances = 0
        call initialize_game(alphabet, drawing)

        do while (chances .LT. 10 .AND. end_game .NE. 1)
            
            correct = 0             ! reset correct to 0

            call get_user_guess(guess, alphabet)
            call check_guess(guess, correct, alphabet, current_word)

            number_guesses = number_guesses + 1

            if (correct .EQ. 1) then
                call print_word(alphabet, current_word, end_game)

                if (end_game .NE. 1) then
                    call get_word_guess(current_word, end_game, number_guesses)
                end if
            else if (correct .EQ. 0) then
                write( *, '(a)' ) "Sorry, that letter isn't in the word"
                chances = chances + 1
                call draw_hangman(chances, drawing)
            end if

        end do

    end subroutine game_loop


    !  check_guess()
    !       Function to check if user guess is correct or not
    subroutine check_guess(guess, correct, alphabet, current_word)
        character guess, alphabet(26), current_word(20)
        integer correct, i, character_value

        ! convert uppercase to lower case
        character_value = ichar(guess)
        if (character_value .GT. 64 .AND. character_value .LT. 90) then
            guess = char(character_value+32)     
            character_value = character_value + 32
        end if

        
        if (character_value .LT. 97 .OR. character_value .GT. 122) then       ! check if character is a valid letter
            WRITE (*,*) "Invalid character"
            correct = -1
        else if (alphabet(character_value - 96) .NE. ' ') then                ! check if character was already guessed
            write( *, '(a)' ) "you already guessed that!"
            correct = -1
            return
        else
            alphabet(character_value - 96) = guess
        endif

        ! loop through word to see if is correct or not
        do i = 1, size(current_word)
            if (current_word(i) .EQ. ' ') then      ! reached end of word
                exit
            end if

            if (ichar(current_word(i)) .EQ. character_value) then
                correct = 1
            end if
        end do
    end subroutine check_guess



    !  get_word_guess()
    !       Prompt user to enter a word guess and test if word is correct word
    subroutine get_word_guess(current_word, end_game, number_guesses)
        character current_word*20
        character word_guess*20
        integer end_game, number_guesses, character_value, i

        write( *, '(a)' ) "What is your guess for the word? (Please only enter lowercase letters)"
        read( *, * ) word_guess

        if (current_word == word_guess) then
            write( *, '(A,I1,A)' ) "Right! It took you ", number_guesses, " guesses"
            end_game = 1
        else
            write( *, '(a)' ) "Wrong. Try another letter"
        end if

    end subroutine get_word_guess




    !  draw_hangman()
    !       Draw the hangman
    subroutine draw_hangman(chances, drawing)
        integer chances, i
        character drawing(12,12)

        select case (chances)
            case (1)
                write ( *, '(a)' ) "First we draw a head."
                drawing(3,6) = "-"; drawing(3,7) = "-"; drawing(3,8) = "-"; drawing(4,5) = "(";
                drawing(4,6) = ".";

                drawing(4,8) = "."; drawing(4,9) = ")"; drawing(5,6) = "-"; drawing(5,7) = "-";
                drawing(5,8) = "-";

            case (2)
                write ( *, '(a)' ) "Now we draw a body."
                do i = 6,9
                    drawing(I,7) = "X"
                end do
            case (3)
                write ( *, '(a)' ) "Next we draw an arm."
                do i = 4,7
                    drawing(i,i-1) = "\\"
                end do
            case (4)
                write ( *, '(a)' ) "This time it's the other arm."
                 drawing(4,11) = "/"; drawing(5,10) = "/"; drawing(6,9) = "/"; drawing(7,8) = "/"
            case (5)
                write ( *, '(a)' ) "Now, let's draw the right leg."
                drawing(10,6) = "/"; drawing(11,5) = "/";
            case (6)
                write ( *, '(a)' ) "This time we draw the left leg."
                drawing(10,8) = "\\"; drawing(11,9) = "\\"
            case (7)
                write ( *, '(a)' ) "Now we put up a hand."
                drawing(3,11) = "\\";
            case (8)
                write ( *, '(a)' ) "Next the other hand."
                drawing(3,3) = "/"
            case (9)
                write ( *, '(a)' ) "Now we draw one foot."
                drawing(12,10) = "\\"; drawing(12,11) = "-";
            case (10)
                write ( *, '(a)' ) "Here's the other foot -- You're hung!!."
                drawing(12,3) = "-"; drawing(12,4) = "/"
        endselect

        do i = 1,12
            do j = 1, 12
                write (*, '(a)', advance='no') drawing(i,j)
            end do
            write ( *, '(a)' ) ' '
        end do
    end subroutine draw_hangman


    !  print_word()
    !       Print all correctly guessed letters, also check if word is completely guessed
    subroutine print_word(alphabet, current_word, end_game)
        character alphabet(26), current_word(20)
        integer i, end_game
        end_game = 1

        do i = 1,size(current_word)
            if (current_word(i) .EQ. ' ') then
                exit
            endif

            if ( alphabet( ichar( current_word(i) ) - 96 ) .NE. ' ' ) then
                write ( *, '(AA$)', advance='no') current_word(i)
            else
                write ( *, '(AA$)', advance='no') '-'
                end_game = 0
            endif
        end do
        write( *, '(a)' ) ' '
    end subroutine print_word


    !  get_user_guess()
    !       Prompt user for next letter guess
    subroutine get_user_guess(guess, alphabet)
        character guess, alphabet(26)

        ! display characters already guessed
        write( *, '(a)' ) "Here are the letters you used: "
        do i = 1,26
            if (alphabet(i) .NE. ' ') then
                WRITE ( *, '(AA$)', advance='no') alphabet(i),","
            end if
        end do
        write( *, '(a)' ) ' '                       ! add newline

        write( *, '(a)' ) 'What is your guess?'
        read( *, * ) guess
    end subroutine get_user_guess



    !  initialize_game()
    !       Set up the alphabet array and the drawing (hangman) array
    subroutine initialize_game(alphabet, drawing)
        integer i, j
        character drawing(12,12), alphabet(26)

        do i = 1,26
            alphabet(i) = ' '
        end do

        do i = 1,12
            do j = 1,12
                drawing(i,j) = " "
            end do
        end do

        !A loop that initializes drawing(i, 1) to 'X'
        do i = 1,12
            drawing(i,1) = "X"
        end do

        !A loop that initializes drawing(1, J) to 'X'
        do j = 1,7
            drawing(1,j) = "X"
        end do
        drawing(2,7) = "X"        !      This is where the man hang froms
        
    end subroutine initialize_game



    !  load_file()
    !       Function to load file and read words for game's dictionary
    subroutine load_file(max_words, words, number_of_words)
        integer max_words, number_of_words
        character*20 words(max_words)
        integer i

        ! file should be passed into command line
        open( 10, file = 'words.txt', status = 'old', iostat = ierr )

        ! test if file opened correctly
        if ( ierr /= 0 ) then
            write( *, '(a)' ) 'file not opened'
            stop
        endif

        ! loop through file and save to words() array
        do i = 1, max_words
            read( 10, fmt=*, iostat = ierr ) words(i)

            if ( ierr > 0 ) then
                write( *, '(a)' ) 'error reading from file ', ierr
                stop
            elseif( ierr < 0 ) then
                ! end of file
                number_of_words = i - 1
                exit
            endif

        enddo

        close( 10 )

    end subroutine load_file

end program hangman
