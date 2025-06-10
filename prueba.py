import subprocess
from pyswip import Prolog
import os
from collections import Counter
import tkinter as tk
from tkinter import messagebox

BASE_FILE = "base_conocimientos.pl"
MOTOR_FILE = "motor_inferencias.pl"

def ordenar_preguntas_por_frecuencia(conocimientos):
    contador = Counter()
    for sintomas in conocimientos.values():
        contador.update(sintomas)
    preguntas_ordenadas = sorted(contador.keys(), key=lambda x: -contador[x])
    return preguntas_ordenadas

def cargar_conocimientos():
    prolog = Prolog()
    prolog.consult(BASE_FILE)
    conocimientos = {}
    for resultado in prolog.query("conocimiento(E, Pregs)"):
        emocion = str(resultado["E"])
        sintomas = [p.decode("utf-8") if isinstance(p, bytes) else str(p) for p in resultado["Pregs"]]
        conocimientos[emocion] = set(sintomas)
    return conocimientos

def ejecutar_motor(respuestas):
    script = "temp_motor_run.pl"
    with open(script, "w", encoding="utf-8") as f:
        f.write(":- consult('{}').\n".format(MOTOR_FILE))
        f.write(":- agregar_respuestas([{}]).\n".format(
            ",".join(['"{}"'.format(r) for r in respuestas])
        ))
        f.write(":- iniciar.\n")
    try:
        resultado = subprocess.check_output(["swipl", "-q", "-f", script], universal_newlines=True)
        return resultado.strip()
    except subprocess.CalledProcessError as e:
        return "Error en la ejecución del motor de inferencia."

def mostrar_resultado(resultado):
    if "|" in resultado:
        emocion, recomendacion = resultado.split("|", 1)
        messagebox.showinfo("Resultado", f"🧠 Diagnóstico: {emocion}\n💡 Recomendación: {recomendacion}")
    else:
        messagebox.showwarning("Resultado", "⚠️ No se pudo determinar un diagnóstico claro.")

class SistemaExpertoGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Sistema Experto Emocional")
        self.conocimientos = cargar_conocimientos()
        self.preguntas = ordenar_preguntas_por_frecuencia(self.conocimientos)
        self.respuestas = []
        self.index = 0

        self.label = tk.Label(root, text="Bienvenido al sistema experto.\nHaz clic en 'Comenzar' para iniciar.", font=("Arial", 14), wraplength=400)
        self.label.pack(pady=20)

        self.boton_comenzar = tk.Button(root, text="Comenzar", command=self.hacer_pregunta, font=("Arial", 12))
        self.boton_comenzar.pack()

        self.frame_botones = tk.Frame(root)
        self.boton_si = tk.Button(self.frame_botones, text="Sí", width=10, command=lambda: self.procesar_respuesta("s"))
        self.boton_no = tk.Button(self.frame_botones, text="No", width=10, command=lambda: self.procesar_respuesta("n"))
        self.boton_si.pack(side=tk.LEFT, padx=10)
        self.boton_no.pack(side=tk.RIGHT, padx=10)

    def hacer_pregunta(self):
        if self.index < len(self.preguntas):
            pregunta = self.preguntas[self.index]
            self.label.config(text=f"{pregunta}?")
            self.boton_comenzar.pack_forget()
            self.frame_botones.pack(pady=10)
        else:
            self.terminar_preguntas()

    def procesar_respuesta(self, respuesta):
        pregunta = self.preguntas[self.index]
        if respuesta == "s":
            self.respuestas.append(pregunta)

            for emocion, sintomas in self.conocimientos.items():
                if sintomas.issubset(set(self.respuestas)):
                    mostrar_resultado(f"{emocion}|Se identificaron todos los síntomas.")
                    self.root.quit()
                    return
        self.index += 1
        self.hacer_pregunta()

    def terminar_preguntas(self):
        self.frame_botones.pack_forget()
        resultado = ejecutar_motor(self.respuestas)
        mostrar_resultado(resultado)
        self.root.quit()

if __name__ == "__main__":
    root = tk.Tk()
    app = SistemaExpertoGUI(root)
    root.mainloop()
