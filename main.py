import subprocess
from pyswip import Prolog
import os

# Rutas relativas a los archivos Prolog
BASE_FILE = "base_conocimientos.pl"
MOTOR_FILE = "motor_inferencias.pl"

# Cargar preguntas desde Prolog
def obtener_preguntas():
    prolog = Prolog()
    prolog.consult(BASE_FILE)
    preguntas = set()
    for resultado in prolog.query("conocimiento(_, Pregs)"):
        for p in resultado["Pregs"]:
            if isinstance(p, bytes):
                preguntas.add(p.decode("utf-8"))
            else:
                preguntas.add(str(p))
    return list(preguntas)

# Preguntar al usuario
def recoger_respuestas(preguntas):
    respuestas = []
    print("Por favor responde con 's' para SÍ o cualquier otra cosa para NO.\n")
    for p in preguntas:
        r = input(p + " ")
        if r.lower() == "s":
            respuestas.append(p)
    return respuestas

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

# Programa principal
if __name__ == "__main__":
    preguntas = obtener_preguntas()
    respuestas = recoger_respuestas(preguntas)
    resultado = ejecutar_motor(respuestas)
    mostrar_resultado(resultado)
