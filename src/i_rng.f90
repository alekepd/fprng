!
!Copyright 2015 University of Chicago
!
!Licensed under the Apache License, Version 2.0 (the "License");
!you may not use this file except in compliance with the License.
!You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
!Unless required by applicable law or agreed to in writing, software
!distributed under the License is distributed on an "AS IS" BASIS,
!WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!See the License for the specific language governing permissions and
!limitations under the License.
!
! Provides an object oriented implementation of an integer pseudo random number generator 
! for fortran 2008.
!
! Not threadsafe.

module prng_i

        use iso_fortran_env, only: int32, int64

        implicit none

        private

        public          :: rng_i_32, xor32, rng_i_64, xor1024star

        !abstract class for single precision random number generator.
        type, abstract                 :: rng_i_32

                private

                contains
                        procedure(seed_number),deferred  :: seed_number
                        procedure(seed_array), deferred  :: seed_array
                        procedure(incr_state), deferred  :: incr_state
                        procedure(sgen_number),deferred  :: sgen_number
                        procedure(sgen_array), deferred  :: sgen_array
                        generic, public    :: sgen => sgen_number, sgen_array
                        generic, public    :: seed => seed_number, seed_array
        end type rng_i_32

        type, abstract                 :: rng_i_64

                private

                contains
                        procedure(r64_seed_array_32),          deferred, private :: seed_32
                        procedure(r64_seed_array_64),          deferred, private :: seed_64
                        procedure(r64_get_seed_array_size_32), deferred, private :: get_seed_array_size_32
                        procedure(r64_get_seed_array_size_64), deferred, private :: get_seed_array_size_64
                        procedure(r64_get_seed_array_size_def),deferred, private :: get_seed_array_size_def
                        procedure(r64_incr_state),             deferred          :: incr_state
                        procedure(r64_sgen_number_32),         deferred, private :: sgen_number_32
                        procedure(r64_sgen_array_32),          deferred, private :: sgen_array_32
                        procedure(r64_sgen_number_64),         deferred, private :: sgen_number_64
                        procedure(r64_sgen_array_64),          deferred, private :: sgen_array_64
                        generic, public    :: sgen => sgen_number_32,&
                                                      sgen_array_32,&
                                                      sgen_array_64,&
                                                      sgen_number_64
                        generic, public    :: seed => seed_64,&
                                                      seed_32
                        generic, public    :: get_seed_array_size => get_seed_array_size_32,&
                                                                     get_seed_array_size_64,&
                                                                     get_seed_array_size_def
        end type rng_i_64

        !simple xorshift 32 bit internal state RNG.
        type, extends(rng_i_32) :: xor32

                private

                integer (int32) :: state

                contains
                        procedure :: seed_number => xor32_seed_number
                        procedure :: seed_array  => xor32_seed_array
                        procedure :: incr_state  => xor32_incr_state
                        procedure :: sgen_number => xor32_sgen_number
                        procedure :: sgen_array  => xor32_sgen_array
        end type xor32

        !xorshift1024* generator. Maximal state space of 2**1024-1.
        type, extends(rng_i_64) :: xor1024star

                private

                integer (int32)           :: seed_array_size    = 16
                integer (int32)           :: seed_array_size_32 = 32

                integer (int32)           :: p
                integer (int64)           :: state(0:15)

                contains
                        procedure :: seed_32                 => xor1024star_seed_32
                        procedure :: seed_64                 => xor1024star_seed_64
                        procedure :: get_seed_array_size_32  => xor1024star_get_seed_array_size_32
                        procedure :: get_seed_array_size_64  => xor1024star_get_seed_array_size_64
                        procedure :: get_seed_array_size_def => xor1024star_get_seed_array_size_def
                        procedure :: incr_state              => xor1024star_incr_state
                        procedure :: sgen_number_32          => xor1024star_sgen_number_32
                        procedure :: sgen_array_32           => xor1024star_sgen_array_32
                        procedure :: sgen_number_64          => xor1024star_sgen_number_64
                        procedure :: sgen_array_64           => xor1024star_sgen_array_64
        end type xor1024star

        abstract interface
            pure subroutine seed_number(self, seed_value)

                import :: rng_i_32, int32

                implicit none

                class(rng_i_32), intent(inout) :: self
                integer (int32), intent(in   ) :: seed_value

            endsubroutine seed_number
        end interface

        abstract interface
            pure subroutine seed_array(self, seed_value, status)

                import :: rng_i_32, int32

                implicit none

                class(rng_i_32),           intent(inout) :: self
                integer (int32),           intent(in   ) :: seed_value(:)
                integer (int32), optional, intent(  out) :: status

            endsubroutine seed_array
        end interface

        abstract interface
            pure subroutine incr_state(self, nsteps)

                import :: rng_i_32, int32

                implicit none

                class(rng_i_32), intent(inout) :: self
                integer (int32), intent(in   ) :: nsteps

            endsubroutine incr_state
        end interface

        abstract interface
            pure subroutine sgen_number(self,rnum)

                import :: rng_i_32, int32

                implicit none

                class(rng_i_32), intent(inout) :: self
                integer (int32), intent(  out) :: rnum

            endsubroutine sgen_number 
        end interface

        abstract interface
            pure subroutine sgen_array(self,rnums)

                import :: rng_i_32, int32

                implicit none

                class(rng_i_32), intent(inout) :: self
                integer (int32), intent(  out) :: rnums(:)

            endsubroutine sgen_array
        end interface

        abstract interface
            pure subroutine r64_seed_array_32(self, seed_value, status)

                import :: rng_i_64, int32, int64

                implicit none

                class(rng_i_64),           intent(inout) :: self
                integer (int32),           intent(in   ) :: seed_value(:)
                integer (int32), optional, intent(inout) :: status

            endsubroutine r64_seed_array_32
        end interface

        abstract interface
            pure subroutine r64_seed_array_64(self, seed_value, status)

                import :: rng_i_64, int32, int64

                implicit none

                class(rng_i_64),           intent(inout) :: self
                integer (int64),           intent(in   ) :: seed_value(:)
                integer (int32), optional, intent(inout) :: status

            endsubroutine r64_seed_array_64
        end interface

        abstract interface
            pure function r64_get_seed_array_size_32(self,sample_int)

                import :: rng_i_64, int32

                implicit none

                class(rng_i_64), intent(in   ) :: self
                integer (int32), intent(in   ) :: sample_int

                integer (int32)                :: r64_get_seed_array_size_32

            endfunction r64_get_seed_array_size_32
        end interface

        abstract interface
            pure function r64_get_seed_array_size_64(self,sample_int)

                import :: rng_i_64, int32, int64

                implicit none

                class(rng_i_64), intent(in   ) :: self
                integer (int64), intent(in   ) :: sample_int

                integer (int32)                :: r64_get_seed_array_size_64

            endfunction r64_get_seed_array_size_64
        end interface

        abstract interface
            pure function r64_get_seed_array_size_def(self)

                import :: rng_i_64, int32, int64

                implicit none

                class(rng_i_64), intent(in   ) :: self
                integer (int32)                :: r64_get_seed_array_size_def

            endfunction r64_get_seed_array_size_def
        end interface

        abstract interface
            pure subroutine r64_incr_state(self, nsteps)

                import :: rng_i_64, int32

                implicit none

                class(rng_i_64), intent(inout) :: self
                integer (int32), intent(in   ) :: nsteps

            endsubroutine r64_incr_state
        end interface

        abstract interface
            pure subroutine r64_sgen_number_32(self,rnum)

                import :: rng_i_64, int32

                implicit none

                class(rng_i_64), intent(inout) :: self
                integer (int32), intent(  out) :: rnum

            endsubroutine r64_sgen_number_32
        end interface

        abstract interface
            pure subroutine r64_sgen_number_64(self,rnum)

                import :: rng_i_64, int64

                implicit none

                class(rng_i_64), intent(inout) :: self
                integer (int64), intent(  out) :: rnum

            endsubroutine r64_sgen_number_64
        end interface

        abstract interface
            pure subroutine r64_sgen_array_32(self,rnums)

                import :: rng_i_64, int32

                implicit none

                class(rng_i_64), intent(inout) :: self
                integer (int32), intent(  out) :: rnums(:)

            endsubroutine r64_sgen_array_32
        end interface

        abstract interface
            pure subroutine r64_sgen_array_64(self,rnums)

                import :: rng_i_64, int64

                implicit none

                class(rng_i_64), intent(inout) :: self
                integer (int64), intent(  out) :: rnums(:)

            endsubroutine r64_sgen_array_64
        end interface

        contains 
                pure subroutine xor32_seed_number(self, seed_value)

                        implicit none

                        class(xor32),     intent(inout) :: self
                        integer (int32),  intent(in   ) :: seed_value

                        self%state = seed_value

                endsubroutine xor32_seed_number

                pure subroutine xor32_seed_array(self, seed_value, status)

                        implicit none

                        class(xor32),              intent(inout) :: self
                        integer (int32),           intent(in   ) :: seed_value(:)
                        integer (int32), optional, intent(  out) :: status

                        if (size(seed_value) == 0) then
                                if (present(status)) status = 1
                                return
                        endif

                        self%state = seed_value(1)

                endsubroutine xor32_seed_array

                pure subroutine xor32_incr_state(self, nsteps)

                        implicit none

                        class(xor32),    intent(inout) :: self
                        integer (int32), intent(in   ) :: nsteps

                        integer (int32)    :: iter

                        do iter=1,nsteps
                                self%state = ieor(self%state,shiftl(self%state,13))
                                self%state = ieor(self%state,shiftr(self%state,17))
                                self%state = ieor(self%state,shiftl(self%state,5))
                        enddo

                endsubroutine xor32_incr_state

                pure subroutine xor32_sgen_number(self, rnum)

                        implicit none

                        class(xor32),    intent(inout) :: self
                        integer (int32), intent(  out) :: rnum

                        call self%incr_state(1)

                        rnum = self%state

                endsubroutine xor32_sgen_number

                pure subroutine xor32_sgen_array(self, rnums)

                        implicit none

                        class(xor32),    intent(inout) :: self
                        integer (int32), intent(  out) :: rnums(:)

                        integer (int32)                :: iter

                        do iter=1,size(rnums)
                                call self%incr_state(1)
                                rnums(iter) = self%state
                        enddo

                endsubroutine xor32_sgen_array

                !!! routines for xor1024star generator.
                pure function xor1024star_get_seed_array_size_32(self,sample_int) result(val)

                        implicit none

                        class(xor1024star), intent(in   ) :: self
                        integer (int32),    intent(in   ) :: sample_int
                        
                        integer (int32) :: val

                        val = self%seed_array_size_32

                endfunction xor1024star_get_seed_array_size_32

                !!! routines for xor1024star generator.
                pure function xor1024star_get_seed_array_size_64(self,sample_int) result(val)

                        implicit none

                        class(xor1024star), intent(in   ) :: self
                        integer (int64),    intent(in   ) :: sample_int
                        
                        integer (int32) :: val

                        val = self%seed_array_size

                endfunction xor1024star_get_seed_array_size_64

                !!! routines for xor1024star generator.
                pure function xor1024star_get_seed_array_size_def(self) result(val)

                        implicit none

                        class(xor1024star), intent(in   ) :: self
                        
                        integer (int32) :: val

                        val = self%seed_array_size

                endfunction xor1024star_get_seed_array_size_def

                !!! routines for xor1024star generator.
                pure subroutine xor1024star_seed_32(self, seed_value, status)

                        implicit none

                        class(xor1024star),           intent(inout) :: self
                        integer (int32),              intent(in   ) :: seed_value(:)
                        integer (int32),    optional, intent(inout) :: status

                        integer (int32) :: iter

                        if (size(seed_value) >= self%seed_array_size_32) then

                                do iter=1,self%seed_array_size
                                        self%state(iter) = &
                                           convert_2_32bit_to_64bit_unsig(seed_value(2*iter-1),&
                                                                          seed_value(2*iter))
                                enddo

                                self%p = 0

                                if (present(status)) status = 0
                        else
                                if (present(status)) status = 1
                        endif

                endsubroutine xor1024star_seed_32


                !!! routines for xor1024star generator.
                pure subroutine xor1024star_seed_64(self, seed_value, status)

                        implicit none

                        class(xor1024star),           intent(inout) :: self
                        integer (int64),              intent(in   ) :: seed_value(:)
                        integer (int32),    optional, intent(inout) :: status

                        if (size(seed_value) >= self%seed_array_size) then
                                self%state = seed_value(1:self%seed_array_size)
                                self%p = 0
                                if (present(status)) status = 0
                        else
                                if (present(status)) status = 1
                        endif

                endsubroutine xor1024star_seed_64

                pure subroutine xor1024star_incr_state(self, nsteps)

                        implicit none

                        class(xor1024star), intent(inout) :: self
                        integer (int32),    intent(in   ) :: nsteps

                        integer (int32)    :: iter

                        integer (int64)              :: s0, s1
                        integer (int64)              :: tmp1, tmp2
                        integer (int32), parameter   :: mask = maskr(4,int32)

                        do iter=1,nsteps
                                s0 = self%state(self%p)
                                self%p = iand((self%p + 1),mask)
                                s1 = self%state(self%p)
                                s1 = ieor(s1,shiftl(s1,31))

                                tmp1 = shiftr(s1,11)
                                tmp2 = shiftr(s0,30)
                                self%state(self%p) = ieor(ieor(s1,s0),ieor(tmp1,tmp2))
                        enddo

                endsubroutine xor1024star_incr_state

                pure subroutine xor1024star_sgen_number_64(self, rnum)

                        implicit none

                        class(xor1024star), intent(inout) :: self
                        integer (int64),    intent(  out) :: rnum

                        !integer (int64),parameter    :: multiplicand = int(1181783497276652981,int64)
                        integer (int64),parameter    :: multiplicand = &
                                int(b'1000001100110100010011101010001010100100101111111110110110101',int64)

                        call self%incr_state(1)

                        rnum = unsigned_64_bit_mult(self%state(self%p),multiplicand)

                endsubroutine xor1024star_sgen_number_64

                pure subroutine xor1024star_sgen_array_64(self, rnums)

                        implicit none

                        class(xor1024star), intent(inout) :: self
                        integer (int64),    intent(  out) :: rnums(:)

                        !integer (int64), parameter   :: multiplicand = int(1181783497276652981,int64)
                        integer (int64), parameter   :: multiplicand = &
                                int(b'1000001100110100010011101010001010100100101111111110110110101',int64)

                        integer (int32) :: iter

                        do iter=1,size(rnums)
                                call self%incr_state(1)
                                rnums(iter) = unsigned_64_bit_mult(self%state(self%p),multiplicand)
                        enddo

                endsubroutine xor1024star_sgen_array_64

                pure subroutine xor1024star_sgen_number_32(self, rnum)

                        implicit none

                        class(xor1024star), intent(inout) :: self
                        integer (int32),    intent(  out) :: rnum

                        integer (int64)                   :: rnum_64

                        call self%sgen_number_64(rnum_64)

                        rnum = int(rnum_64,int32)

                endsubroutine xor1024star_sgen_number_32

                pure subroutine xor1024star_sgen_array_32(self, rnums)

                        implicit none

                        class(xor1024star), intent(inout) :: self
                        integer (int32),    intent(  out) :: rnums(:)

                        integer (int64),allocatable       :: rnums_64(:)

                        allocate(rnums_64(size(rnums)))

                        call self%sgen_array_64(rnums_64)

                        rnums = int(rnums_64,int32)

                endsubroutine xor1024star_sgen_array_32

                !given two numbers in signed 64 bit format, return signed 64 bit int 
                !which contains binary content of the corresponding 64 bit unsigned.
                pure function unsigned_64_bit_add(num1, num2) result(core_bits)

                        implicit none

                        integer (int64), intent(in   ) :: num1, num2

                        integer (int64) :: core_bits, add_bits, save_bits

                        core_bits = num1
                        add_bits  = num2

                        do while (add_bits /= maskr(0,int64))
                                save_bits = iand(core_bits,add_bits)
                                core_bits = ieor(core_bits,add_bits)
                                add_bits  = save_bits
                                add_bits  = shiftl(add_bits,1)
                        enddo

                endfunction unsigned_64_bit_add

                !given two numbers in signed 64 bit format, return signed 64 bit int 
                !which contains binary content of the corresponding 64 bit unsigned.
                pure function unsigned_64_bit_mult(num1, num2) result(prod)

                        implicit none

                        integer (int64), intent(in   ) :: num1, num2

                        integer (int64) :: prod, num1_mut, num2_mut

                        num1_mut = num1
                        num2_mut = num2

                        prod = maskr(0,int64)

                        do while (num2_mut /= maskr(0,int64))
                                if (btest(num2_mut,0)) then
                                        prod = unsigned_64_bit_add(prod,num1_mut)
                                endif
                                num2_mut = shiftr(num2_mut,1)
                                num1_mut = shiftl(num1_mut,1)
                        enddo

                endfunction unsigned_64_bit_mult

                pure function convert_2_32bit_to_64bit_unsig(small1, small2) result(big)

                        implicit none

                        integer (int32), intent(in   ) :: small1, small2

                        integer (int64) :: big, big_temp

                        big      = int(small1,int64)
                        big_temp = int(small2,int64)

                        call mvbits(big_temp,0,32,big,32)

                endfunction convert_2_32bit_to_64bit_unsig

endmodule prng_i
