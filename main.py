import cv2

# Preguntas y etiquetas
preguntas = {
    "ansiedad": [
        "¿Te cuesta dormir últimamente?",
        "¿Te sientes inquieto o acelerado?"
    ],
    "tristeza": [
        "¿Te sientes sin ganas de hacer cosas?",
        "¿Lloras con frecuencia sin razón clara?"
    ],
    "estres": [
        "¿Sientes mucha presión por tus responsabilidades?",
        "¿Tienes dolores físicos sin causa médica clara?"
    ]
}

respuestas = []

recomendaciones = {
    "ansiedad": "Puede que estés con ansiedad leve. Trata de hablar con alguien de confianza o caminar al aire libre.",
    "tristeza": "Podrías estar pasando por tristeza. Intenta escribir cómo te sientes o hablar con alguien cercano.",
    "estres": "Podrías tener estrés acumulado. Prueba ejercicios de respiración o pausas activas.",
    "ninguno": "No se pudo determinar un estado claro. Considera buscar ayuda profesional."
}

def mostrar_pantalla(texto, altura=400, anchura=700):
    fondo = 255 * np.ones((altura, anchura, 3), dtype=np.uint8)
    y = 80
    for linea in texto.split('\n'):
        cv2.putText(fondo, linea, (40, y), cv2.FONT_HERSHEY_SIMPLEX, 0.9, (0, 0, 0), 2)
        y += 50
    return fondo

def preguntar(pregunta):
    pantalla = mostrar_pantalla(f"{pregunta}\n\nPresiona 's' para Sí, 'n' para No.")
    cv2.imshow("MentalCare", pantalla)
    key = cv2.waitKey(0)
    return chr(key) == 's'

def diagnosticar(respuestas):
    conteo = {"ansiedad": 0, "tristeza": 0, "estres": 0}
    for tag, preguntas_tag in preguntas.items():
        for p in preguntas_tag:
            if p in respuestas:
                conteo[tag] += 1

    max_estado = max(conteo, key=conteo.get)
    if conteo[max_estado] >= 2:
        return max_estado
    return "ninguno"

def mostrar_resultado(estado):
    mensaje = f"Diagnóstico: {estado.upper()}\n\nSugerencia:\n{recomendaciones[estado]}"
    pantalla = mostrar_pantalla(mensaje, altura=500)
    cv2.imshow("MentalCare", pantalla)
    cv2.waitKey(0)

import numpy as np

def main():
    for tag, lista_preguntas in preguntas.items():
        for p in lista_preguntas:
            if preguntar(p):
                respuestas.append(p)

    estado = diagnosticar(respuestas)
    mostrar_resultado(estado)
    cv2.destroyAllWindows()

if __name__ == "__main__":
    main()
