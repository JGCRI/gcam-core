// <disable4786.h>
#ifndef DISABLE4786
#define DISABLE4786
#if defined(_MSC_VER) && _MSC_VER <= 1200
#pragma warning(disable:4786) // NOTE: May be able to get by just with this line in .NET.
// http://support.microsoft.com/support/kb/articles/Q167/3/55.ASP
// states that yes, it's a compiler bug that
// #pragma warning(disable: 4786) doesn't always work.
// They don't, however, list a workaround.
// I found that, very strangely, #including <iostream> made the 
// remaining 4786 warnings go away!
// Of course, #including <iostream> is inefficient and
// slows compilation - so I whittled away most of what's in
// <iostream> and discovered that the "active ingredient" in
// <iostream> appears to be a declaration of a static class,
// complete with default constructor.
// For some reason, this works around the bug!
// Why does this work? Beats me, ask those smart guys at MS who 
// wrote the compiler.
class msVC6_4786WorkAround {
public:
msVC6_4786WorkAround() {}
};
static msVC6_4786WorkAround WowIWonderWhatCrapCodeMustBeInTheCompilerToMakeThisWorkaroundWork;
#endif
#endif
