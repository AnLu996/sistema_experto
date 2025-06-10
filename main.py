import tkinter as tk
from tkinter import messagebox
from PIL import Image, ImageTk, ImageSequence
import json
import subprocess

# Archivos de configuración
BASE_CONOCIMIENTOS_FILE = "base_conocimientos.pl"
MOTOR_FILE = "motor_inferencias.pl"
PREGUNTAS_FILE = "gui_info.json"

# Cargar preguntas y GIFs
with open(PREGUNTAS_FILE, "r", encoding="utf-8") as f:
    preguntas_gifs = json.load(f)

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

def mostrar_resultado(resultado):
    if "|" in resultado:
        emocion, recomendacion = resultado.split("|", 1)
        messagebox.showinfo("🧠 Diagnóstico", f"💬 Estado emocional: {emocion.capitalize()}\n\n💡 Recomendación: {recomendacion}")
    else:
        messagebox.showwarning("Sin diagnóstico", "⚠️ No se pudo determinar un estado emocional claro.")

class SistemaExpertoGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("💬 Sistema Experto Emocional")
        self.root.geometry("720x480")
        self.root.configure(bg="#222")

        self.preguntas = list(preguntas_gifs.keys())
        self.respuestas = []
        self.index = 0
        self.frames = []
        self.animando = False

        # --- Portada con imagen de fondo ---
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

        # Texto y botón sobre el fondo
        self.titulo_portada = self.canvas_portada.create_text(
            360, 80, text="DIAGNÓSTICO EMOCIONAL", font=("Helvetica", 30, "bold"), fill="white"
        )
        self.texto_portada = self.canvas_portada.create_text(
            360, 350,
            text="Responde las siguientes preguntas para recibir una recomendación personalizada.",
            font=("Helvetica", 16), fill="black", width=600
        )
        # Botón "Comenzar" sobre imagen con window_create
        self.boton_comenzar = tk.Button(
            self.root, text="🚀 Comenzar", font=("Helvetica", 15, "bold"),
            bg="#4CAF50", fg="white", width=18, command=self.comenzar
        )
        self.canvas_portada.create_window(360, 420, window=self.boton_comenzar)

        # --- Frame principal de preguntas (GIF como fondo) ---
        self.frame_main = tk.Frame(self.root, bg="#222", width=720, height=480)

        self.canvas_bg = tk.Canvas(self.frame_main, width=720, height=480, highlightthickness=0, bg="#222")
        self.canvas_bg.place(x=0, y=0, relwidth=1, relheight=1)

        # Los widgets van encima del canvas, sin transparencia
        self.label_pregunta = tk.Label(self.frame_main, text="", font=("Helvetica", 16, "bold"),
                                       bg="white", fg="#222", wraplength=560, justify="center")
        self.label_pregunta.place(relx=0.5, rely=0.18, anchor="center")

        self.frame_botones = tk.Frame(self.frame_main, bg="white")
        self.boton_si = tk.Button(self.frame_botones, text="✅ Sí", font=("Helvetica", 13, "bold"),
                                 bg="#2196F3", fg="", width=10, height=2, command=lambda: self.responder(True))
        self.boton_no = tk.Button(self.frame_botones, text="❌ No", font=("Helvetica", 13, "bold"),
                                 bg="#f44336", fg="white", width=10, height=2, command=lambda: self.responder(False))
        self.boton_si.pack(side=tk.LEFT, padx=30)
        self.boton_no.pack(side=tk.RIGHT, padx=30)
        self.frame_botones.place(relx=0.5, rely=0.32, anchor="center")

        self.boton_reiniciar = tk.Button(self.frame_main, text="🔄 Reiniciar", font=("Helvetica", 11),
                                         bg="#FF9800", fg="white", command=self.reiniciar)

    def comenzar(self):
        self.frame_portada.pack_forget()
        self.frame_main.pack(fill=tk.BOTH, expand=True)
        self.respuestas = []
        self.index = 0
        self.boton_reiniciar.place_forget()
        self.hacer_pregunta()

    def hacer_pregunta(self):
        if self.index < len(self.preguntas):
            pregunta = self.preguntas[self.index]
            self.label_pregunta.config(text=pregunta)
            self.cargar_gif_fondo(preguntas_gifs[pregunta])
            self.frame_botones.place(relx=0.5, rely=0.32, anchor="center")
            self.boton_reiniciar.place_forget()
        else:
            self.terminar()

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
            self.respuestas.append(self.preguntas[self.index])
        self.index += 1
        self.hacer_pregunta()

    def terminar(self):
        self.frame_botones.place_forget()
        self.animando = False
        resultado = ejecutar_motor(self.respuestas)
        mostrar_resultado(resultado)
        self.boton_reiniciar.place(relx=0.5, rely=0.45, anchor="center")

    def reiniciar(self):
        self.frame_main.pack_forget()
        self.comenzar()

if __name__ == "__main__":
    root = tk.Tk()
    app = SistemaExpertoGUI(root)
    root.mainloop()