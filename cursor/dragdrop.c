/* cc -lX11 -lXtst -o xdrag{,.c} */
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>

int main (int argc, char** argv)
{
    Display* dpy = XOpenDisplay(NULL);    
    int x0 = atoi(argv[1]), y0 = atoi(argv[2]), 
        x1 = atoi(argv[3]), y1 = atoi(argv[4]);
    
    XTestFakeMotionEvent(dpy, -1, x0, y0, CurrentTime);
    XTestFakeButtonEvent(dpy, 1, True, 80);
    XTestFakeMotionEvent(dpy, -1, x1, y1, 150);
    XTestFakeButtonEvent(dpy, 1, False, 80);
    XFlush(dpy);
    XCloseDisplay(dpy);
    return 0;
}
