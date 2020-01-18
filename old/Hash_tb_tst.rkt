#lang racket

 

(define comm-ch-dir-ops-hsh-tb (hash
                                'RUP   'prog-dir-Rw-Up
                                'RDW   'prog-dir-Rw-Dw
                                'RLF   'prog-dir-Rw-Lf
                                'RRT   'prog-dir-Rw-Rt
                                'PUP   'prog-dir-Pg-Up
                                'PDW   'prog-dir-Pg-Dw
                                ))


(define comm-single-ops-hsh-tb (hash
                             'STP 'prog-row-halt
                             'STO 'prog-rndm-halt

                             'NEA 'add-one-prog-mem-A-ptr
                             'PEA 'sub-one-prog-mem-A-ptr
                             'NEB 'add-one-prog-mem-B-ptr
                             'PEB 'sub-one-prog-mem-B-ptr
                             'SWP 'swap-prog-mem-ptr-A-B
                             'CPA 'copy-val-at-A-into-B
                             'CPB 'copy-val-at-B-into-A
                             
                             'OIA 'display-val-int-at-A
                             'OIB 'display-val-int-at-B
                             'OAA 'display-val-ascii-at-A
                             'OAB 'display-val-ascii-at-B
                             'IIA 'input-val-int-at-A
                             'IIB 'input-val-int-at-B

                             'AOA 'incr-mem-by-one-at-ptr-A
                             'AOB 'incr-mem-by-one-at-ptr-B
                             'SOA 'decr-mem-by-one-at-ptr-A
                             'SOB 'decr-mem-by-one-at-ptr-B
                             'REA 'reset-mem-val-at-ptr-A
                             'REB 'reset-mem-val-at-ptr-B
                             'ADA 'add-val-to-A
                             'ADB 'add-val-to-B
                             'SBA 'sub-val-to-A
                             'SBB 'sub-val-to-B
                             ))


(define comm-cntrl-ops-hsh-tb (hash
                                'CAZ   'prog-chck-A-z
                                'CBZ   'prog-chck-B-z
                                'CAL   'prog-chck-A-l
                                'CBL   'prog-chck-B-l
                                'CIE   'prog-chck-AB-eq
                                ))





(hash-ref comm-single-ops-hsh-tb 'NEA)




