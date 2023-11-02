from flask import Flask, render_template, send_file
import os
import random
import sys
actr_usability_path = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) + "/actr7.x/usability"
#print(actr_usability_path)
#actr_usability_path = "/home/donghee/src/wde-usability/actr7.x/usability"
sys.path.append(actr_usability_path)

from run_usability_system import draw_usability_graph
app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/usability')
def interactivity():
    return f'<h4>{random.randint(80, 100)}</h4>'

@app.route('/fig')
def fig_inter_force():
    #img = draw_usability_graph(10, ["gait", "adaptive"], 30, 30, 5)
    img = draw_usability_graph(10, ["gait", "adaptive"], 30, 5, 5)
    return send_file(img, mimetype='image/png')

if __name__ == '__main__':
     app.run(host='0.0.0.0', port=5054, debug=True)
