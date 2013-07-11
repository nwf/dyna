import pylab as pl
from matplotlib.animation import FuncAnimation
from collections import defaultdict

class draw(object):
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

    def __init__(self, interp):
        self.interp = interp

    def main(self):

        frame = defaultdict(list)
        for _, [t, item], val in self.interp.chart['frame/2'][:,:,:]:
            if val:
                frame[t].append(item)

        if not frame:
            print 'no frames found.'
            return

        nframes = max(frame)

        assert nframes == 0, 'many frames found. Did you mean to use an animtation?'

        def draw_frame(t):
            ax.cla()
            ax.grid(False)
            if t not in frame:
                print 'frame', t, 'missing.'
            for item in frame[t]:
                fn = item. fn

                if fn == 'title/1':
                    [title] = item.args
                    ax.set_title(title)

                elif fn == 'xlim/2':
                    [a,b] = item.args
                    ax.set_xlim(a,b)

                elif fn == 'ylim/2':
                    [a,b] = item.args
                    ax.set_ylim(a,b)

                elif fn == 'line/2':
                    [(a,b), (c,d)] = item.args
                    ax.plot([a,c], [b,d], color='b', alpha=0.5)

                elif fn == 'text/2':
                    (s,(x,y)) = item.args
                    ax.text(x,y,s)

                else:
                    print 'dont know how to render', item

        fig = pl.figure(figsize=(10,10))
        ax = pl.axes()

        draw_frame(0)
        
        #pl.ion(); pl.show()
        #from arsenal.debug import ip; ip()
        pl.show()
