import subprocess
from pyswip import Prolog
import os
from collections import Counter

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

def recoger_respuestas_dinamico(preguntas, conocimientos):
    respuestas = []
    print("Por favor responde con 's' para SÍ o cualquier otra cosa para NO.\n")

    for p in preguntas:
        r = input(p + " ")
        if r.lower() == "s":
            respuestas.append(p)

            # Verifica si ya se cumple alguna emoción
            for emocion, sintomas in conocimientos.items():
                if sintomas.issubset(set(respuestas)):
                    print(f"\n✅ Todos los síntomas de '{emocion}' se han confirmado.")
                    return respuestas  # Se detiene aquí si ya hay diagnóstico completo

    return respuestas

# Cargar preguntas desde Prolog
def obtener_preguntas(conocimientos):
    return ordenar_preguntas_por_frecuencia(conocimientos)

# Ejecutar Prolog con las respuestas
def ejecutar_motor(respuestas):
    # Crear un archivo temporal que ejecute el motor con las respuestas
    script = "temp_motor_run.pl"
    with open(script, "w", encoding="utf-8") as f:
        f.write(":- consult('{}').\n".format(MOTOR_FILE))
        f.write(":- agregar_respuestas([{}]).\n".format(
            ",".join(['"{}"'.format(r) for r in respuestas])
        ))
        f.write(":- iniciar.\n")
    
    # Ejecutar el archivo con SWI-Prolog
    try:
        resultado = subprocess.check_output(["swipl", "-q", "-f", script], universal_newlines=True)
        return resultado.strip()
    except subprocess.CalledProcessError as e:
        return "Error en la ejecución del motor de inferencia."

# Mostrar resultado
def mostrar_resultado(resultado):
    if "|" in resultado:
        emocion, recomendacion = resultado.split("|", 1)
        print(f"\n🧠 Diagnóstico: {emocion}")
        print(f"💡 Recomendación: {recomendacion}")
    else:
        print("\n⚠️ No se pudo determinar un diagnóstico claro.")

if __name__ == "__main__":
    conocimientos = cargar_conocimientos()
    preguntas = obtener_preguntas(conocimientos)
    respuestas = recoger_respuestas_dinamico(preguntas, conocimientos)
    resultado = ejecutar_motor(respuestas)
    mostrar_resultado(resultado)
