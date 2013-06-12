import pylab as pl
from matplotlib.animation import FuncAnimation
from collections import defaultdict

def g(nodes, edges, t, ax, interp):
    ax.cla()
    ax.set_title(t)
    ax.set_xlim(-2,2)
    ax.set_ylim(-2,2)

    pos = defaultdict(lambda: (0,0))
    pos.update({node: p for _, (node, _), p in interp.chart['pos/2'][:,t,:]})

    for u,v in edges:
        if u < v:
            (a,b), (c,d) = pos[u], pos[v]
            ax.plot([a,c], [b,d])

    for s in nodes:
        x,y = pos[s]
        ax.text(x,y,s)

def animate(interp):
    [(_, _, niter)] = interp.chart['niter/0'][:,]

    nodes = [name for _, [name], _ in interp.chart['node/1'][:,:]]
    edges = [(u,v) for _, [u,v] ,_ in interp.chart['edge/2'][:,:,:]]

    fig = pl.figure()
    ax = pl.axes()

    print 'creating animation..'
    anim = FuncAnimation(fig, lambda t: g(nodes, edges, t % niter, ax, interp), frames=niter)
    print 'saving...'
    anim.save('examples/force.dyna.mp4', fps=30, extra_args=['-vcodec', 'libx264'])
    print 'wrote examples/force.dyna.mp4'


def main(interp):
    animate(interp)
