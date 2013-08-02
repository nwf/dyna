import argparse
from IPython.external.path import path
from errors import DynaCompilerError
from errors import crash_handler
from interpreter import Interpreter
from repl import REPL
from config import dotdynadir, dynahome
import post, load

def main():
    parser = argparse.ArgumentParser(description="The dyna interpreter!")

    parser.add_argument('--version', action='store_true',
                        help='Print version information.')
    parser.add_argument('source', nargs='*', type=path,
                        help='Path to Dyna source file.')
    parser.add_argument('-i', dest='interactive', action='store_true',
                        help='Fire-up REPL after runing solver..')
    parser.add_argument('-o', '--output', dest='output',
                        type=argparse.FileType('wb'),
                        help='Write solution to file.')
    parser.add_argument('--post-process', nargs='*',
                        help='run post-processor.')
    parser.add_argument('--load', nargs='*',
                        help='run loaders.')
    parser.add_argument('--debug', action='store_true',
                        help='Debug planner, normalizer and parser.')

    args = parser.parse_args()

    if args.version:
        try:
            print (dynahome / 'VERSION').text()
        except IOError:
            print 'failed to obtain version info.'
        exit(0)

    interp = Interpreter()

    crash_handler()

    if args.source:

        if len(args.source) > 1:
            # concatenate files
            with file(interp.tmp / 'tmp.dyna', 'wb') as g:
                for f in args.source:
                    if not f.exists():
                        print 'File `%s` does not exist.' % f
                        return
                    with file(f) as f:
                        g.write('\n')
                        g.write('%'*80)
                        g.write('\n')
                        g.write('%% ')
                        g.write(f.name)
                        g.write('\n')
                        g.write(f.read())
            args.source = g.name
        else:
            [args.source] = args.source

        if not args.source.exists():
            print 'File `%s` does not exist.' % args.source
            return

        if args.debug:
            import debug
            debug.main(args.source, browser=True)
            exit(1)

        try:
            plan = interp.dynac(args.source)
        except DynaCompilerError as e:
            print e
            exit(1)

        interp.load_plan(plan)
        interp.run_agenda()

    if args.load:
        for cmd in args.load:
            load.run(interp, cmd)

    if args.post_process:
        for cmd in args.post_process:
            post.run(interp, cmd)

    if args.load or args.post_process or args.source:
        interp.dump_charts(args.output)      # should be a post-processor

    if args.interactive or not args.source:
        repl = REPL(interp)

        def repl_crash():
            # all files the interpreter generated
            with file(dotdynadir / 'crash-repl.log', 'wb') as f:
                for line in repl.lines:
                    print >> f, line

        crash_handler.hooks.append(repl_crash)

        repl.cmdloop()


if __name__ == '__main__':
    main()
