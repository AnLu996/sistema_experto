import tkinter as tk
from PIL import Image, ImageTk, ImageSequence
import json
import subprocess
from pyswip import Prolog

BASE_FILE = "base_conocimientos.pl"
MOTOR_FILE = "motor_inferencias.pl"
PREGUNTAS_FILE = "gui_info.json"

with open(PREGUNTAS_FILE, "r", encoding="utf-8") as f:
    preguntas_gifs = json.load(f)

def cargar_conocimientos_ordenados():
    prolog = Prolog()
    prolog.consult(BASE_FILE)
    conocimientos = []
    for resultado in prolog.query("conocimiento(E, Pregs)"):
        emocion = str(resultado["E"])
        sintomas = [p.decode("utf-8") if isinstance(p, bytes) else str(p) for p in resultado["Pregs"]]
        conocimientos.append((emocion, sintomas))
    return conocimientos

def ejecutar_motor(respuestas):
    script = "temp_motor_run.pl"
    with open(script, "w", encoding="utf-8") as f:
        f.write(f":- consult('{MOTOR_FILE}').\n")
        f.write(":- agregar_respuestas([{}]).\n".format(
            ",".join([f'"{r}"' for r in respuestas])
        ))
        f.write(":- iniciar.\n")
    try:
        resultado = subprocess.check_output(["swipl", "-q", "-f", script], universal_newlines=True)
        return resultado.strip()
    except subprocess.CalledProcessError:
        return "Error en la ejecución del motor de inferencia."

def mostrar_resultado_bonito(root, resultado):
    for widget in root.winfo_children():
        if isinstance(widget, tk.Toplevel) and widget.title() == "🧠 Diagnóstico":
            widget.destroy()
    ventana = tk.Toplevel(root)
    ventana.title("🧠 Diagnóstico")
    ventana.geometry("420x320")
    ventana.configure(bg="#232946")
    ventana.resizable(False, False)
    ventana.transient(root)
    ventana.grab_set()
    bg = tk.Canvas(ventana, width=420, height=320, bg="#232946", highlightthickness=0)
    bg.place(x=0, y=0, relwidth=1, relheight=1)
    bg.create_rectangle(20, 20, 400, 300, fill="#fffffe", outline="#232946", width=0)
    bg.create_text(210, 60, text="🧠", font=("Helvetica", 54), fill="#f6c90e")
    if "|" in resultado:
        emocion, recomendacion = resultado.split("|", 1)
        bg.create_text(210, 115, text="Estado emocional:", font=("Helvetica", 14, "bold"), fill="#232946")
        bg.create_text(210, 145, text=emocion.capitalize(), font=("Helvetica", 21, "bold"), fill="#3081f7")
        bg.create_text(210, 185, text="Recomendación:", font=("Helvetica", 13, "bold"), fill="#232946")
        bg.create_text(210, 220, text=recomendacion, font=("Helvetica", 13), fill="#232946", width=340)
    else:
        bg.create_text(210, 150, text="⚠️ No se pudo determinar un estado emocional claro.", font=("Helvetica", 14), fill="#f44336", width=340)
    btn = tk.Button(ventana, text="Cerrar", font=("Helvetica", 13, "bold"),
                    bg="#3081f7", fg="white", width=12, command=ventana.destroy, borderwidth=0, activebackground="#255dc9")
    btn.place(x=160, y=260)
    ventana.update_idletasks()
    x = root.winfo_x() + (root.winfo_width() // 2 - 210)
    y = root.winfo_y() + (root.winfo_height() // 2 - 160)
    ventana.geometry(f"+{x}+{y}")

class SistemaExpertoGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Sistema Experto Emocional")
        self.root.geometry("720x480")
        self.root.configure(bg="#222")

        self.diagnosticos = cargar_conocimientos_ordenados()
        self.respuestas = []
        self.diagnostico_idx = 0
        self.sintoma_idx = 0
        self.frames = []
        self.animando = False

        self.frame_portada = tk.Frame(self.root, width=720, height=480)
        self.frame_portada.pack(fill=tk.BOTH, expand=True)
        self.canvas_portada = tk.Canvas(self.frame_portada, width=720, height=480, highlightthickness=0)
        self.canvas_portada.pack(fill=tk.BOTH, expand=True)
        try:
            portada_img = Image.open("gifs/portada.png").resize((720, 480), Image.LANCZOS)
            self.imagen_portada = ImageTk.PhotoImage(portada_img)
            self.bg_portada = self.canvas_portada.create_image(0, 0, anchor="nw", image=self.imagen_portada)
        except Exception as e:
            self.imagen_portada = None
            print("No se pudo cargar la imagen de portada:", e)
        self.titulo_portada = self.canvas_portada.create_text(
            360, 80, text="DIAGNÓSTICO EMOCIONAL", font=("Helvetica", 30, "bold"), fill="white"
        )
        self.texto_portada = self.canvas_portada.create_text(
            360, 350,
            text="Responde las siguientes preguntas para recibir una recomendación personalizada.",
            font=("Helvetica", 16), fill="black", width=600
        )
        self.boton_comenzar = tk.Button(
            self.root, text="🚀 Comenzar", font=("Helvetica", 15, "bold"),
            bg="#4CAF50", fg="white", width=18, command=self.comenzar
        )
        self.canvas_portada.create_window(360, 420, window=self.boton_comenzar)

        self.frame_main = tk.Frame(self.root, bg="#222", width=720, height=480)
        self.canvas_bg = tk.Canvas(self.frame_main, width=720, height=480, highlightthickness=0, bg="#222")
        self.canvas_bg.place(x=0, y=0, relwidth=1, relheight=1)
        self.label_pregunta = tk.Label(self.frame_main, text="", font=("Helvetica", 16, "bold"),
                                       fg="#222", wraplength=560, justify="center")
        self.label_pregunta.place(x=360, y=50, anchor="center")
        self.frame_botones = tk.Frame(self.frame_main)
        self.boton_si = tk.Button(self.frame_botones, text="✅ Sí", font=("Helvetica", 14, "bold"),
                                  bg="#2196F3", fg="white", width=15, height=2, command=lambda: self.responder(True))
        self.boton_no = tk.Button(self.frame_botones, text="❌ No", font=("Helvetica", 14, "bold"),
                                  bg="#f44336", fg="white", width=15, height=2, command=lambda: self.responder(False))
        self.boton_si.pack(pady=(0, 40))
        self.boton_no.pack()
        self.frame_botones.place(x=510, y=240, anchor="center")
        self.boton_reiniciar = tk.Button(self.frame_main, text="🔄 Reiniciar", font=("Helvetica", 12),
                                         bg="#FF9800", fg="white", command=self.reiniciar)

    def comenzar(self):
        self.frame_portada.pack_forget()
        self.frame_main.pack(fill=tk.BOTH, expand=True)
        self.respuestas = []
        self.diagnostico_idx = 0
        self.sintoma_idx = 0
        self.boton_reiniciar.place_forget()
        self.hacer_pregunta()

    def hacer_pregunta(self):
        # Si no quedan diagnósticos, terminar
        if self.diagnostico_idx >= len(self.diagnosticos):
            self.terminar()
            return
        emocion, sintomas = self.diagnosticos[self.diagnostico_idx]
        # Si ya no quedan sintomas en este diagnóstico, mostrar resultado
        if self.sintoma_idx >= len(sintomas):
            # Si ya contestó SÍ a todos los síntomas: mostrar diagnóstico
            resultado = ejecutar_motor(self.respuestas)
            if "|" in resultado and not resultado.startswith("ninguno"):
                self.frame_botones.place_forget()
                self.animando = False
                mostrar_resultado_bonito(self.root, resultado)
                self.boton_reiniciar.place(relx=0.5, rely=0.9, anchor="center")
                return
            else:
                # Si no, pasar al siguiente diagnóstico
                self.diagnostico_idx += 1
                self.sintoma_idx = 0
                self.hacer_pregunta()
                return
        pregunta = sintomas[self.sintoma_idx]
        self.pregunta_actual = pregunta
        self.label_pregunta.config(text=pregunta)
        self.cargar_gif_fondo(preguntas_gifs.get(pregunta, "ruta_default.gif"))
        self.frame_botones.place(x=510, y=240, anchor="center")
        self.boton_reiniciar.place_forget()

    def cargar_gif_fondo(self, gif_path):
        self.frames = []
        self.animando = False
        try:
            im = Image.open(gif_path)
            for frame in ImageSequence.Iterator(im):
                frame = frame.resize((720, 480))
                self.frames.append(ImageTk.PhotoImage(frame))
            self.animando = True
            self.mostrar_frame_fondo(0)
        except Exception as e:
            print("No se pudo cargar el GIF:", e)
            self.canvas_bg.delete("all")
            self.canvas_bg.create_rectangle(0,0,720,480, fill="#333", outline="")

    def mostrar_frame_fondo(self, idx):
        if self.animando and self.frames:
            self.canvas_bg.delete("all")
            self.canvas_bg.create_image(0, 0, anchor="nw", image=self.frames[idx])
            next_idx = (idx + 1) % len(self.frames)
            self.root.after(100, self.mostrar_frame_fondo, next_idx)

    def responder(self, es_si):
        if es_si:
            self.respuestas.append(self.pregunta_actual)
            self.sintoma_idx += 1  # Siguiente síntoma de este diagnóstico
        else:
            # Si responde NO a cualquier síntoma, descarta diagnóstico entero y pasa al siguiente
            self.diagnostico_idx += 1
            self.sintoma_idx = 0
        self.hacer_pregunta()

    def terminar(self):
        self.frame_botones.place_forget()
        self.animando = False
        resultado = ejecutar_motor(self.respuestas)
        mostrar_resultado_bonito(self.root, resultado)
        self.boton_reiniciar.place(relx=0.5, rely=0.9, anchor="center")

    def reiniciar(self):
        self.frame_main.pack_forget()
        self.comenzar()

if __name__ == "__main__":
    root = tk.Tk()
    app = SistemaExpertoGUI(root)
    root.mainloop()