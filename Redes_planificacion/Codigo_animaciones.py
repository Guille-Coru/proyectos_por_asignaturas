from manim import *
import numpy as np
config.background_color=WHITE;Text.set_default(color=BLACK)
class graprueba(Scene):
    def construct(self):
        laos=ImageMobject("laos_provinces.png")
       

        vertices = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
        edges = [(0,3),(0,4),(1,2),(1,3),(2,6),(2,3),(3,4),(3,6),(4,5),(4,6),(4,7),(4,8),(5,8),
                 (6,7),(7,8),(7,9),(8,9),(9,10),(10,11),(11,12),(12,13),(12,14),(13,14),(13,15),(14,15)]
        lt= {0: [-1.4,3.1,0], 1: [-2.45,2.45,0], 2: [-2.9,1.9,0], 3: [-1.75,1.9,0], 4: [-1.,1.75,0],
             5: [0.1,2,0], 6: [-2.1,0.82,0], 7: [-1.35,0.1,0], 8: [-0.4,1.1,0], 9: [0.45,0.2,0], 10: [1.24,-0.6,0],
             11: [1.5,-1.5,0], 12: [1.9,-2.25,0], 13: [1.7,-3.3,0], 14: [2.7,-2.4,0], 15: [2.65,-3.2,0]
             }
        g = Graph(vertices, edges, labels=True, vertex_config={"fill_color": BLACK}, label_fill_color=WHITE,
                  edge_config={"stroke_color": BLACK},layout=lt)
        regi = Tex("REGIÓN TRIPLE",color=BLACK).scale(0.5).move_to([-1.35,-1.5,0])
        arrow=Line(start =regi.get_top(),end=g[7].get_bottom(),buff=0.05,color=BLACK).add_tip()
        self.add(laos.scale(2.5))
        self.wait()
        self.add(g)
        self.wait()
        self.add(regi,arrow)
        self.wait(duration=4)
        self.remove(laos,regi,arrow)
        self.wait(duration=3)
        # newp={0: [0.75,2,0],1: [-0.75,2,0],2: [-2.25,1,0],3: [-0.75,1,0],4: [0.75,1,0],
        #        5: [2.25,2,0],6: [-0.75,0,0],7: [0.75,0,0],8: [2.25,1,0],9: [2.25,0,0],
        #        10: [2.25,-1,0],11: [0.75,-1,0],12: [-0.75,-2,0],13: [-0.75,-1,0],14: [-2.25,-1,0],15: [-2.25,0,0]}

        #####1.2
        # newp={0: [0.9,2.4,0],1: [-0.9,2.4,0],2: [-2.7,1.2,0],3: [-0.9,1.2,0],4: [0.9,1.2,0],
        #        5: [2.7,2.4,0],6: [-0.9,0,0],7: [0.9,0,0],8: [2.7,1.2,0],9: [2.7,0,0],
        #        10: [2.7,-1.2,0],11: [0.9,-1.2,0],12: [-0.9,-2.4,0],13: [-0.9,-1.2,0],14: [-2.7,-1.2,0],15: [-2.7,0,0]} 
        #####1.4
        newp={0: [1.05,2.8,0],1: [-1.05,2.8,0],2: [-3.15,1.4,0],3: [-1.05,1.4,0],4: [1.05,1.4,0],
               5: [3.15,2.8,0],6: [-1.05,0,0],7: [1.05,0,0],8: [3.15,1.4,0],9: [3.15,0,0],
               10: [3.15,-1.4,0],11: [1.05,-1.4,0],12: [-1.05,-2.8,0],13: [-1.05,-1.4,0],14: [-3.15,-1.4,0],15: [-3.15,0,0]}
          
        self.play(
            g[0].animate.move_to(newp[0]),
            g[1].animate.move_to(newp[1]),
            g[2].animate.move_to(newp[2]),
            g[3].animate.move_to(newp[3]),
            g[4].animate.move_to(newp[4]),
            g[5].animate.move_to(newp[5]),
            g[6].animate.move_to(newp[6]),
            g[7].animate.move_to(newp[7]),
            g[8].animate.move_to(newp[8]),
            g[9].animate.move_to(newp[9])
            ,rate=linear)
        for i in range(10, 15):
            self.wait(duration=0.5)
            self.play(g[i].animate.move_to([newp[i]]))
            if i==15:
                break
        self.play(g[15].animate.move_to(newp[15]))
        self.wait(duration=3)      
class gravuelta(Scene):
    def construct(self):
        laos=ImageMobject("laos_provinces.png")
        vertices = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
        edges = [(0,3),(1,2),(1,3),(3,4),(4,6),(4,8),(5,8),
                (7,9),(8,9),(9,10),(10,11),(11,12),(12,13),(12,14),(14,15)]
        newp2={0: [1.05,2.8,0],1: [-1.05,2.8,0],2: [-3.15,1.4,0],3: [-1.05,1.4,0],4: [1.05,1.4,0],
                5: [3.15,2.8,0],6: [-1.05,0,0],7: [1.05,0,0],8: [3.15,1.4,0],9: [3.15,0,0],
                10: [3.15,-1.4,0],11: [1.05,-1.4,0],12: [-1.05,-2.8,0],13: [-1.05,-1.4,0],14: [-3.15,-1.4,0],15: [-3.15,0,0]}
        gv = Graph(vertices, edges, labels=True, vertex_config={"fill_color": BLACK}, label_fill_color=WHITE,
                    edge_config={"stroke_color": BLACK},layout=newp2)
        oldp= {0: [-1.4,3.1,0], 1: [-2.45,2.45,0], 2: [-2.9,1.9,0], 3: [-1.75,1.9,0], 4: [-1.,1.75,0],
                5: [0.1,2,0], 6: [-2.1,0.82,0], 7: [-1.35,0.1,0], 8: [-0.4,1.1,0], 9: [0.45,0.2,0], 10: [1.24,-0.6,0],
                11: [1.5,-1.5,0], 12: [1.9,-2.25,0], 13: [1.7,-3.3,0], 14: [2.7,-2.4,0], 15: [2.65,-3.2,0]
                }
        gvp=Graph(vertices, edges, labels=True, vertex_config={"fill_color": BLACK}, label_fill_color=WHITE,
                    edge_config={"stroke_color": BLACK},layout=oldp)
        self.add(gv)
        self.wait(duration=2)
        for i in range(10, 15):
                self.wait(duration=0.5)
                self.play(gv[i].animate.move_to([oldp[i]]))
                if i==15:
                    break
        self.play(gv[15].animate.move_to(oldp[15]))
        self.wait(duration=0.5)
        self.play(
            gv[0].animate.move_to(oldp[0]),
            gv[1].animate.move_to(oldp[1]),
            gv[2].animate.move_to(oldp[2]),
            gv[3].animate.move_to(oldp[3]),
            gv[4].animate.move_to(oldp[4]),
            gv[5].animate.move_to(oldp[5]),
            gv[6].animate.move_to(oldp[6]),
            gv[7].animate.move_to(oldp[7]),
            gv[8].animate.move_to(oldp[8]),
            gv[9].animate.move_to(oldp[9])
            ,rate=linear)
        #self.play(remove(gv),run_time=0.001)
        self.bring_to_back(laos.scale(2.5))
        self.wait(duration=3)