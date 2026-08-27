# Native suspension lowering selection

Decision: **select the direct iterative state machine**.

Every candidate passed the frozen semantic and structural hard gates. The approved rule selects an LLVM candidate only when it has at least one material advantage and no material regression; otherwise direct wins.

| candidate metric | direct | candidate | delta | noise/size threshold | result |
| --- | ---: | ---: | ---: | ---: | --- |
| switched compileO0 | 31.120479 | 33.046374 | 1.925895 (6.19%) | 1.852458 | not material |
| switched compileO2 | 32.597751 | 36.851250 | 4.253499 (13.05%) | 2.314082 | **material regression** |
| switched resumeO2PerBoundary | 0.001133 | 0.001150 | 0.000017 (1.53%) | 0.000240 | not material |
| switched frameBytes | 24.000000 | 40.000000 | 16.000000 (66.67%) | 16.000000 | **material regression** |
| switched linkedCodeDataBytes | 6396.000000 | 6572.000000 | 176.000000 (2.75%) | 639.600000 | not material |
| retcon compileO0 | 31.120479 | 32.350875 | 1.230396 (3.95%) | 2.494375 | not material |
| retcon compileO2 | 32.597751 | 36.119188 | 3.521437 (10.80%) | 2.410791 | **material regression** |
| retcon resumeO2PerBoundary | 0.001133 | 0.001144 | 0.000011 (0.97%) | 0.000275 | not material |
| retcon frameBytes | 24.000000 | 64.000000 | 40.000000 (166.67%) | 16.000000 | **material regression** |
| retcon linkedCodeDataBytes | 6396.000000 | 6662.000000 | 266.000000 (4.16%) | 639.600000 | not material |

Switched-resume has no material advantage and regresses O2 compilation and frame size. That frame result triggered the bounded returned-continuation experiment. Retcon also has no material advantage and increases the allocator-visible frame from 24 to 64 bytes. Both LLVM candidates are rejected.

The rejected `llvm.coro.*` constructions remain reproducible, disposable fixtures in this directory; no production compiler or `@silk-lang/llvm` surface was added. Production task 5.1 therefore implements direct iterative lowering behind target-neutral continuation descriptors. Exact raw operands and machine-readable decisions are in [evidence.json](evidence.json).
