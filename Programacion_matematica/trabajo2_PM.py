from manim import *
import numpy as np
config.background_color=WHITE;Text.set_default(color=BLACK)
class superficieP4(ThreeDScene):
    def func(self,u,v):
        return np.array([u,v,(1/12)*(4-u*v)])
        #return np.array([u,v,2*np.cos(np.sqrt(u**2+v**2))+(np.sqrt(u**2+v**2))/(6)-np.log(u**2+u*np.sin(u-v)+3)])
    def construct(self):
        self.set_camera_orientation(phi=PI/3.32,theta=-PI/3)

        axes=ThreeDAxes(x_range=[-0.4,2.1],y_range=[-0.4,2.1],z_range=[-0.2,2.5])
        labels=axes.get_axis_labels(
            x_label="x_1",
            y_label="x_2",
            z_label="f(x_1,x_2)")
        axes.set_color(BLACK)
        labels.set_color(BLACK)
        superf=Surface(
            lambda u,v: axes.c2p(*self.func(u,v)),
            u_range=(0,2),
            v_range=(0,2),
            resolution=100,
            fill_opacity=0.75,
            #checkerboard_colors=[BLUE,YELLOW]
                            )
        # cjto=VGroup(axes,labels,superf)
        # self.add(cjto)
        # axes.scale(0.65)
        # superf.scale(0.65)
        cjto=VGroup(axes,labels,superf)
        # cjto.to_edge(UL)
        # cjto.scale(0.65)
        cjto.move_to(ORIGIN+UP+LEFT)
        self.add(cjto)
        
        # self.wait(2)
        # # self.play(cjto.animate.scale(0.65))
        # self.begin_ambient_camera_rotation(rate=PI/20)
        # self.wait(10)
        # self.stop_ambient_camera_rotation()
