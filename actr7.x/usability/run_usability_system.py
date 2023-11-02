################################
#
# RUN USABILITY SYSTEM MODULE
#
# Hyungseok Oh
#
################################
#
#

import actr
import math
import matplotlib.pyplot as plt
import numpy as np


actr.load_act_r_code("actr7.x/usability/system_interface.lisp")

def run_usability_system(time, task, age, force, noise):
    total_usability_value = 0
    learnability = 0
    memorability = 0
    utility = 0
    efficiency = 0
    effectiveness = 0
    performance_time = 0
    #error_value = 0
    model_output_text = None

    cog_level = "high"

    actr.reset()

    actr.load_act_r_code("actr7.x/usability/1.0/model_parameters.lisp")

    if task[0] == "arm":
        actr.load_act_r_code("actr7.x/usability/1.0/task/simple_arm_movement_task.lisp")
    elif task[0] == "gait":
        actr.load_act_r_code("actr7.x/usability/1.0/task/simple_gait_task.lisp")



    ##setting parameters
    input_twin_age_parameter(age)
    input_task_information_parameter(task)
    input_twin_cognitive_ability_parameter(cog_level)
    input_device_force_timeline_parameter(force)
    input_twin_noise_parameter(noise)
    input_task_performance_time_parameter(time)

    setting_input_parameters_for_model()


    ##run model
    if task[0] == "arm":
        model_output_text = run_arm_model_until_time(time)
    elif task[0] == "gait":
        model_output_text = run_gait_model_until_time(time)
        

    learnability = get_value_learnability_parameter()
    memorability = get_value_memorability_parameter()
    utility = get_value_utility_parameter()
    efficiency = get_value_mental_workload_parameter()
    effectiveness = get_value_effectiveness_parameter()
    performance_time = time

    total_usability_value = (learnability + utility + efficiency + effectiveness) / 4

    make_usability_graph([total_usability_value, learnability, utility, efficiency, effectiveness])

    return [math.floor(total_usability_value), [memorability, utility, efficiency, effectiveness], performance_time, get_value_ACT_R_model_output()]


def make_usability_graph (usability):
    x = np.arange(len(usability))
    usability_name = ["USABILITY", "LEARNABILITY", "UTILITY", "EFFICIENCY", "EFFECTIVENESS"]

    plt.bar(x, usability)
    plt.xticks(x, usability_name)


def draw_usability_graph ():
    plt.show()
    



def get_usability_value_from_model (time, task, age, cog_level, trajectory, force, noise):
    return actr.current_connection.evaluate_single("run-the-model-until-time", time, task, age, cog_level, trajectory, force, noise)
    



def output_total_model_parameters ():
    return actr.current_connection.evaluate_single("output-total-model-parameters")

def run_gait_model_until_time (time):

    init_utility_test("gait")

    window = actr.open_exp_window("GAIT", visible=False)
    actr.add_text_to_exp_window(window, "O", x=50, y=50)
    actr.add_text_to_exp_window(window, "X", x=50, y=75)

    actr.install_device(window)

    actr.run(time, real_time=False)

    return output_total_model_parameters()


def run_arm_model_until_time (time):

    init_utility_test("arm")

    #actr.install_device(window)

    actr.run(time, real_time=False)

    return output_total_model_parameters()



def init_utility_test (task):
    if task == "arm":
        actr.current_connection.evaluate_single("init-arm-test")
    elif task == "gait":
        actr.current_connection.evaluate_single("init-gait-test")


def setting_input_parameters_for_model ():
    return actr.current_connection.evaluate_single("setting-input-parameters")


def input_twin_age_parameter (value):
    return actr.current_connection.evaluate_single("input-twin-age-parameter", value)

def get_value_twin_age ():
    return actr.current_connection.evaluate_single("get-value-twin-age")



def input_task_information_parameter (value):
    return actr.current_connection.evaluate_single("input-task-information-parameter", value)

def get_value_task_information ():
    return actr.current_connection.evaluate_single("get-value-task-information")



def input_twin_cognitive_ability_parameter (value):
    return actr.current_connection.evaluate_single("input-twin-cognitive-ability-parameter", value)

def get_value_twin_cognitive_ability ():
    return actr.current_connection.evaluate_single("get-value-twin-cognitive-ability")
        


def input_device_force_timeline_parameter (value):
    return actr.current_connection.evaluate_single("input-device-force-timeline-parameter", value)

def get_value_device_force_timeline ():
    return actr.current_connection.evaluate_single("get-value-device-force-timeline")



def input_task_performance_time_parameter (value):
    return actr.current_connection.evaluate_single("input-task-performance-time-parameter", value)

def get_value_task_performance_time ():
    return actr.current_connection.evaluate_single("get-value-task-performance-time")



def input_twin_noise_parameter (value):
    return actr.current_connection.evaluate_single("input-twin-noise-parameter", value)

def get_value_twin_noise ():
    return actr.current_connection.evaluate_single("get-value-twin-noise")


##
##  OUTPUT PARAMETERS
##

def input_ACT_R_model_output (value):
    return actr.current_connection.evaluate_single("input-ACT-R-model-output", value)

def get_value_ACT_R_model_output ():
    return actr.current_connection.evaluate_single("get-value-ACT-R-model-output")



def input_mental_workload_parameter (value):
    return actr.current_connection.evaluate_single("input-mental-workload-parameter", value)

def get_value_mental_workload_parameter ():
    return actr.current_connection.evaluate_single("get-value-mental-workload-parameter")



def input_human_error_parameter (value):
    return actr.current_connection.evaluate_single("input-human-error-parameter", value)

def get_value_human_error_parameter ():
    return actr.current_connection.evaluate_single("get-value-human-error-parameter")



def input_learnability_parameter (value):
    return actr.current_connection.evaluate_single("input-learnability-parameter", value)

def get_value_learnability_parameter ():
    return actr.current_connection.evaluate_single("get-value-learnability-parameter")



def input_memorability_parameter (value):
    return actr.current_connection.evaluate_single("input-memorability-parameter", value)

def get_value_memorability_parameter ():
    return actr.current_connection.evaluate_single("get-value-memorability-parameter")



def input_utility_parameter (value):
    return actr.current_connection.evaluate_single("input-utility-parameter", value)

def get_value_utility_parameter ():
    return actr.current_connection.evaluate_single("get-value-utility-parameter")




def input_effectiveness_parameter (value):
    return actr.current_connection.evaluate_single("input-effectiveness-parameter", value)

def get_value_effectiveness_parameter ():
    return actr.current_connection.evaluate_single("get-value-effectiveness-parameter")



def input_total_usability_parameter (value):
    return actr.current_connection.evaluate_single("input-total-usability-parameter", value)

def get_value_total_usability_parameter ():
    return actr.current_connection.evaluate_single("get-value-total-usability-parameter")
