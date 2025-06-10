import cv2
import numpy as np
import subprocess

# Preguntas directamente mapeadas al Prolog
preguntas = [
    "¿Te cuesta dormir últimamente?",
    "¿Te sientes inquieto o acelerado?",
    "¿Te sientes sin ganas de hacer cosas?",
    "¿Lloras con frecuencia sin razón clara?",
    "¿Sientes mucha presión por tus responsabilidades?",
    "¿Tienes dolores físicos sin causa médica clara?"
]

respuestas_afirmativas = []

def mostrar_pantalla(texto, altura=400, anchura=800):
    fondo = 255 * np.ones((altura, anchura, 3), dtype=np.uint8)
    y = 100
    for linea in texto.split('\n'):
        cv2.putText(fondo, linea, (50, y), cv2.FONT_HERSHEY_SIMPLEX, 0.9, (50, 50, 50), 2)
        y += 50
    return fondo

def preguntar(pregunta):
    pantalla = mostrar_pantalla(f"{pregunta}\n\nPresiona 's' para Sí o 'n' para No.")
    cv2.imshow("MentalCare", pantalla)
    key = cv2.waitKey(0)
    return chr(key) == 's'

def escribir_script_respuestas(respuestas):
    script = ":- consult('motor_inferencias.pl').\n"
    for r in respuestas:
        script += f":- agregar_respuesta(\"{r}\").\n"
    script += ":- iniciar.\n"

    with open("consulta.pl", "w", encoding="utf-8") as f:
        f.write(script)

def obtener_diagnostico():
    result = subprocess.run(["swipl", "-q", "-f", "consulta.pl"], capture_output=True, text=True)
    return result.stdout.strip()

def mostrar_resultado(texto):
    diagnostico, recomendacion = texto.split("|")
    mensaje = f"Diagnóstico: {diagnostico.upper()}\n\nSugerencia:\n{recomendacion}"
    pantalla = mostrar_pantalla(mensaje, altura=500)
    cv2.imshow("MentalCare", pantalla)
    cv2.waitKey(0)
    cv2.destroyAllWindows()

def main():
    for p in preguntas:
        if preguntar(p):
            respuestas_afirmativas.append(p)

    escribir_script_respuestas(respuestas_afirmativas)
    resultado = obtener_diagnostico()
    mostrar_resultado(resultado)

if __name__ == "__main__":
    main()
