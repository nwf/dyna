import pylab as pl
from matplotlib.animation import FuncAnimation
from collections import defaultdict
from stdlib import topython
import numpy as np

class graph(object):
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

    def main(self, outfile, fps=30):
    
        frame = defaultdict(list)
        for _, [t, item], val in self.interp.chart['frame/2'][:,:,:]:
            if val:
                frame[t].append(item)

        nframes = max(frame)

        def draw_frame(t):
            
            # Makes a call to pl.draw() in order to obtain a renderer from matplotlib
            pl.draw()
            renderer = ax.get_renderer_cache()


            ax.cla()
            ax.set_title(t)
            ax.set_xlim(-2,2)   # TODO: this isn't right...
            ax.set_ylim(-2,2)
            if t not in frame:
                print 'frame', t, 'missing.'
            for item in frame[t]:

                print "DEBUG: ", item.args


                if item.fn == 'line/2':
                    [(a,b), (c,d)] = map(topython, item.args)
                    outputPlot = ax.plot([a,c], [b,d], color='b', alpha=0.5)
                    
                    

                elif item.fn == 'text/2':

                    # s is the text to print. x,y position
                    (s,(x,y)) = map(topython, item.args)
                    
                    outputText = ax.text(x,y,s)
                    textBbox = outputText.get_window_extent(renderer)
                    
                    # Examples of ways you can access the bbox data.
                    #print "textBbox: ", textBbox
                    #print "textBbox coordinates: ",[(textBbox.x0, textBbox.y0),(textBbox.x1, textBbox.y1)]
                    #print "textBbox.corners(): ", textBbox.corners()
                    
                    # Neat thing I found. You can drop right into the iPython shell to play around or debug.
                    #from IPython import embed
                    #embed()
                    
                else:
                    print 'dont know how to render', item



        fig = pl.figure()
        ax = pl.axes()


        
        #anim = FuncAnimation(fig, draw_frame, frames=nframes)
        anim = FuncAnimation(fig, draw_frame)
        

        #anim.save(outfile, fps=15)
        pl.show()
        
