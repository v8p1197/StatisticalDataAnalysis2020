fit.linear <- (overall~recommended+seat_comfort+cabin_service+food_bev+entertainment+ground_service
               +wifi_connectivity+value_for_money)

fit.poly2.complete <- (overall~recommended+poly(seat_comfort,2)+poly(cabin_service,2)
                       +poly(food_bev,2)+poly(entertainment,2)+poly(ground_service,2)
                       +poly(wifi_connectivity,2)+poly(value_for_money,2))

fit.poly2 <- (overall~recommended+ground_service+seat_comfort+wifi_connectivity
              +value_for_money+I(entertainment^2)+I(seat_comfort^2)+I(food_bev^2)+I(ground_service^2)
              +I(cabin_service^2)+I(value_for_money^2))

fit.poly3 <- (overall~recommended+poly(seat_comfort,3)+poly(cabin_service,3)
              +poly(food_bev,3)+poly(entertainment,3)+poly(ground_service,3)
              +poly(wifi_connectivity,3)+poly(value_for_money,3))

fit.poly4 <- (overall~recommended+poly(seat_comfort,4)+poly(cabin_service,4)
              +poly(food_bev,4)+poly(entertainment,4)+poly(ground_service,4)
              +poly(wifi_connectivity,4)+poly(value_for_money,4))


fit.log <- (overall~recommended+log(seat_comfort)+log(cabin_service)
            +log(food_bev)+log(entertainment)+log(ground_service)+log(value_for_money))