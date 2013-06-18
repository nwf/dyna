"""
Postprocessor for animated visualization of basic elements such as lines and
text.

We look for the following patterns in the dynabase

         visual element
               v
    frame(T, &text(String, tuple(X, Y))).
          ^
       time index

Frames should have value true. The example above places a text element reading
`String` at position `(X,Y)` in a frame at time `T`. This element can be
specified by dyna rule.
"""

import pylab as pl
from matplotlib.animation import FuncAnimation
from collections import defaultdict

class graph(object):

    def __init__(self, interp):
        self.interp = interp

    def main(self, outfile):
        
        frame = defaultdict(list)
        for _, [t, item], val in self.interp.chart['frame/2'][:,:,:]:
            if val:
                frame[t].append(item)
    
        nframes = max(frame)
    
        def draw_frame(t):
            ax.cla()
            ax.set_title(t)
            ax.set_xlim(-2,2)   # TODO: this isn't right...
            ax.set_ylim(-2,2)
            if t not in frame:
                print 'frame', t, 'missing.'
            for item in frame[t]:
                if item.fn == 'line/2':
                    [(a,b), (c,d)] = item.args
                    ax.plot([a,c], [b,d], color='b', alpha=0.5)
                elif item.fn == 'text/2':
                    (s,(x,y)) = item.args
                    ax.text(x,y,s)
                else:
                    print 'dont know how to render', item
    
        fig = pl.figure()
        ax = pl.axes()
    
        print 'creating animation..'
        anim = FuncAnimation(fig, draw_frame, frames=nframes)
        print 'saving...'
        anim.save(outfile, fps=30, extra_args=['-vcodec', 'libx264'])
        print 'wrote examples/force.dyna.mp4'
