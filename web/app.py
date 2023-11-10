from flask import Flask, render_template, send_file, request
import os
import random
import sys
actr_usability_path = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) + "/actr7.x/usability"
sys.path.append(actr_usability_path)

from run_usability_system import draw_usability_graph
app = Flask(__name__)

@app.route('/')
def index():
    device = 'upper' if request.args.get('device') == None else request.args.get('device')
    body = 'body1' if request.args.get('body') == None else request.args.get('body')
    joint = 'joint1' if request.args.get('joint') == None else request.args.get('joint')
    code = 'code1' if request.args.get('code') == None else request.args.get('code')
    url = f'/?device={device}&body={body}&joint={joint}&code={code}'
    age = 30
    return render_template('index.html', url=url, device=device, body=body, age=age, code=code)

@app.route('/information')
def information():
    device = request.args.get('device')
    body = request.args.get('body')
    task = request.args.get('task')
    code = request.args.get('code')
    return render_template('information.html', device=device, body=body, task=task, code=code)

@app.route('/score/usability')
def interactivity():
    device = request.args.get('device')
    if device == "upper":
        task = "arm"
    elif device == "lower":
        task = "gait"
    else:
        task = "arm" # default
 
    age = request.args.get('age')
    code = request.args.get('code')

    error_rate = 0
    if code == 'code1':
        error_rate = 10
    elif code == 'code2':
        error_rate = 35
    elif code == 'code3':
        error_rate = 80

    return f"""
    <div id="usability-result" hx-swap-oob="true" hx-swap="outerHTML">
    <div class="flex justify-center py-12">
    <img id="update-spinner" class="htmx-indicator" src="https://htmx.org/img/bars.svg"/ width=200>
    <img id="usability-result-fig" src="fig/usability/{task}/{age}/{error_rate}"/>
    </div>
    <div>
    <p>
    그래프 수치에 대한 설명
    </p>
    </div>
    </div>
    """

@app.route('/fig/usability/<task>/<age>/<error_rate>')
def fig_inter_force(task, age, error_rate):
    timeout = 10
    age = int(age)
    force = int(error_rate)
    noise = 5
    img = draw_usability_graph(timeout, [task, "adaptive"], age, force, noise)
    #  img = draw_usability_graph(10, ["gait", "adaptive"], 30, 5, 5)
 
    return send_file(img, mimetype='image/png')

if __name__ == '__main__':
     app.run(host='0.0.0.0', port=5050, debug=True)
