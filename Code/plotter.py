import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg


with open("input.txt", "r") as file:
  for line in file: 
    if "C" in line:
      cells = line.split(" ")
      for cell in cells:
        if "." in cell:
          C = cell
    if "Nx" in line:
      cells = line.split(" ")
      cell = cells[1]
      Nx = cell.strip()
    if "adv_vl" in line:
      cells = line.split(" ")
      cell = cells[1]
      adv_vl = cell
    if "type" in line:
      cells = line.split(" ")
      cell = cells[1]
      type_ = cell.strip()

if type_ == str(0):
  def plot_advection(adv_file):

      with open(adv_file, "r") as file:
        lines = file.readlines()
        xs = lines[0].split(" ")
        xs_list = []
        for x in xs:
          if x == '':
            continue
          if "." in x.strip():
            xs_list.append(float(x.strip()))

        u_init = lines[1].split(" ")
        uinit_list = []
        for u in u_init:
          if u == '':
            continue
          if "." in u.strip():
            uinit_list.append(float(u.strip()))

        u_final = lines[-1].split(" ")
        ufinal_list = []
        for uf in u_final:
          if uf == '':
            continue
          if "." in uf.strip():
            ufinal_list.append(float(uf.strip()))


        plt.subplot(2, 3, i+1)

        plt.plot(xs_list[1: int(Nx)+1], ufinal_list, label = "Final Solution", color = "blue")
        plt.plot(xs_list[1: int(Nx)+1], uinit_list, label = "Initial Conditions", color = "red")

        plt.yticks(fontsize=5)
        plt.xticks([xs_list[0], xs_list[-1]], fontsize=5)

        if "discrete_upwind" in adv_file:
          title = "Discrete FTBS " + "(N=" + str(Nx) + ")"
        if "discrete_laxwendroff" in adv_file:
          title = "Discrete Lax-Wendroff "+ "(N=" + str(Nx) + ")"
        if "discrete_secondcenter" in adv_file:
          title = "Discrete FTCS "+ "(N=" + str(Nx) + ")"
        if "cont_upwind" in adv_file:
          title = "Continuous FTBS "+ "(N=" + str(Nx) + ")"
        if "cont_laxwendroff" in adv_file:
          title = "Continuous Lax-Wendroff "+ "(N=" + str(Nx) + ")"
        if "cont_secondcenter" in adv_file:
          title = "Continuous FTCS "+ "(N=" + str(Nx) + ")"


        plt.title(title)
        plt.legend()

  plt.figure(figsize=(10, 6))
  files = ["discrete_upwind", "discrete_laxwendroff", "discrete_secondcenter", "cont_upwind", "cont_laxwendroff", "cont_secondcenter"]
  plt.figure(figsize=(15, 10))  # Adjust the figure size according to your preference
  for i, file in enumerate(files):
    print("-------------------------------" + file + "-----------------------------")
    if Nx.strip() == "32":
      file = file + "_N=" + str(Nx).strip() + " _C=" + str(C).strip() + ".dat"
    else: 
      file = file + "_N=" + str(Nx).strip() + "_C=" + str(C).strip() + ".dat"

    plot_advection(file)
  plt.tight_layout()  # Adjust layout for better spacing
  plt.savefig("advection_subplots_N=" + Nx.strip() + "_C=" + str(C).strip() + ".jpg")

if type_ == str(1): #diffusion
  def plot_diff(initial_text, file_text, ratio):
    x_initial = initial_text[1]
    u_initial = initial_text[0]
    x = file_text[1]
    u = file_text[0]
    print(Nx)

    plt.plot(u_initial, x_initial, label = "Initial Conditions", color = "grey")
    label_ = str(ratio) + "%"
    plt.plot(x,u, label = label_, marker = "o", markersize = 3, color = "blue")
    plt.xticks([x[0], x[-1]], fontsize=5)
    plt.title("1D Diffusion " + label_)
    plt.legend()
    plt.savefig("1D Diffusion " + label_+ "_Nx=" + Nx +  ".jpg")
    plt.close()


  for ratio in [0]:  
    file_name = "diff_" + str(ratio) + "_N=" +str(Nx).strip() + ".dat"
    if Nx.strip() == "32":
      file_name = "diff_" + str(ratio) + "_N=" +str(Nx).strip() + " .dat"

    with open(file_name, "r") as file:
      print("plotting: " + file_name)
      initial_text = []
      for line in file:
        line_items = []
        for item in line.split(" "):
          if item == '':
            continue
          if "." in item:
            line_items.append(float(item))
        initial_text.append(line_items)

  timestep_ratio = [20, 50, 80, 100]
  for ratio in timestep_ratio:  
    file_name = "diff_" + str(ratio) + "_N=" + str(Nx).strip() + ".dat"
    if Nx.strip() == "32":
      file_name = "diff_" + str(ratio) + "_N=" +str(Nx).strip() + " .dat"
    with open(file_name, "r") as file:
      print("plotting: " + file_name)
      file_text = []
      for line in file:
        line_items = []
        for item in line.split(" "):
          if item == '':
            continue
          if "." in item:
            line_items.append(float(item))
          file_text.append(line_items)
        plot_diff(initial_text, file_text, ratio)



  def diff_subplot(image_files):
      num_files = len(image_files)
      num_rows = 2
      num_cols = 2
      fig, axs = plt.subplots(num_rows, num_cols, figsize=(10, 8), sharex=True, gridspec_kw={'hspace': 0, 'wspace': 0}, subplot_kw={'aspect': 'auto'})

      for i, image_file in enumerate(image_files):
          img = mpimg.imread(image_file)
          row_index = i // num_cols
          col_index = i % num_cols
          axs[row_index, col_index].imshow(img, extent=[0, 1, 0, 1], aspect='auto')
          axs[row_index, col_index].set_axis_off()

      axs[-1, 0].set_xlabel("x")
      axs[-1, 1].set_xlabel("x")
      axs[0, 0].set_ylabel("u")
      axs[1, 0].set_ylabel("u")

      plt.tight_layout()
      plt.savefig("diff_subplots" + "_Nx=" + str(int(Nx.strip())) + ".jpg")
      plt.close
  plots = ["1D Diffusion 20%" + "_Nx=" + str(int(Nx.strip())) +  ".jpg", "1D Diffusion 50%" + "_Nx=" + str(int(Nx.strip())) + ".jpg", "1D Diffusion 80%" + "_Nx=" + str(int(Nx.strip())) + ".jpg", "1D Diffusion 100%" + "_Nx=" + str(int(Nx.strip())) + ".jpg"]
  diff_subplot(plots)
  